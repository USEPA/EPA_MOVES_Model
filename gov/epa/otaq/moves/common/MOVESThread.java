/**************************************************************************************************
 * @(#)MOVESThread.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.Iterator;
import java.util.LinkedList;

/**
 * Implements the basic functionality needed to manage the threads instantiated by a MOVES Master
 * or Worker. A static member variable is used to keep track of all the threads started by an
 * application.
 * 
 * @author		Wesley Faler
 * @version		2009-04-04
**/
public abstract class MOVESThread extends Thread {
	/** Contains all currently running MOVESThread instances. **/
	static LinkedList<MOVESThread> activeThreads = new LinkedList<MOVESThread>();

	/** This flag signals this thread to exit. This directly affects only one thread. **/
	volatile boolean signaledToExit = false;
	/** While this count is greater than 0, this thread should pause operations **/
	volatile int suspendCount = 0;

	/**
	 * Used exclusively for allowing a parent thread to track exceptions that occur in child
	 * threads. 
	**/
	MOVESThread parent = null;

	/**
	 * Count of the number of exceptions caught in the MOVESThread class. This is inteneded for
	 * JUnit test case assertion purposes. Under release execution this will be ignored.
	 * This includes exceptions from child threads that have were passed this as a parent
	 * parameter in the constructor.
	**/
	int exceptionCount = 0;

	/**
	 * This returns if the thread is signaled to exit.
	 * This is safer than reading the instance variable directly since this value is modified
	 * across threads and current Java implementations don't reliably support the volatile
	 * modifier.
	 * @return Is this thread signaled to exit?
	**/
	public synchronized boolean isSignaledToExit() {
		return signaledToExit;
	}

	/**
	 * This returns the thread's suspend count.
	 * This is safer than reading the instance variable directly since this value is modified
	 * across threads and current Java implementations don't reliably support the volatile
	 * modifier.
	 * @return The thread's suspend count.
	**/
	synchronized int getSuspendCount() {
		return suspendCount;
	}

	/**
	 * Gets the number of exceptions caught in this class. This count doesn't include exceptions
	 * caught in parent or child classes. This is intended for JUnit purposes only.
	 * @return The exception count.
	**/
	public int getExceptionCount() {
		return exceptionCount;
	}

	/**
	 * Signals all instances of MOVESThread to terminate and then returns immediately without
	 * any wait.
	**/
	public static void signalAllToTerminate() {
		LinkedList<MOVESThread> tempThreads = new LinkedList<MOVESThread>();
		synchronized (activeThreads) {
			tempThreads.addAll(activeThreads);
		}

		Iterator<MOVESThread> threadIterator = tempThreads.iterator();

		while (threadIterator.hasNext()) {
			MOVESThread iterThread = (MOVESThread)threadIterator.next();
			iterThread.signalToTerminate();
		}
	}

	/**
	 * Invokes signalAllToTerminate and waits until all threads have actually exited.
	 * @throws InterruptedException This will be thrown if the running thread is interrupted
	 * while waiting.
	**/
	public static void signalAllToTerminateAndWait() throws InterruptedException {
		signalAllToTerminate();

		synchronized (activeThreads) {
			while (activeThreads.size() > 0) {
				activeThreads.wait();
			}
		}
	}

	/**
	 * Basic constructor.
	**/
	public MOVESThread() {

	}

	/**
	 * Basic constructor.
	 * @param name The name of the thread.
	**/
	public MOVESThread(String name) {
		super(name);
	}

	/**
	 * Basic constructor.
	 * @param parent The parent thread. Used for exception count tracking purposes.
	 * @param name The name of the thread.
	**/
	public MOVESThread(MOVESThread parent, String name) {
		super(name);

		this.parent = parent;
	}

	/** Signals this thread to terminate and then returns immediately without waiting. **/
	public synchronized void signalToTerminate() {
		signaledToExit = true;

		notifyAll();
		interrupt();
	}

	/**
	 * Signals this thread to terminate and then waits for the termination to complete.
	 * @throws InterruptedException This will be thrown if the running thread is interrupted
	 * while waiting.
	**/
	public void signalToTerminateAndWait() throws InterruptedException {
		signalToTerminate();

		join();
	}

	/**
	 * Increments the suspend count so that this thread will be signaled to pause operations.
	 * The pause will occur at the thread's convenience; typically when no locks are held.
	**/
	public synchronized void signalToSuspend() {
		suspendCount++;
		notifyAll();
	}
	
	/**
	 * Decrements the suspend count. When equal to zero, the thread will continue execution
	 * from a signalToSuspend() operation. It is illegal to call this when already zero.
	**/
	public synchronized void signalToResume() {
		// JDK 1.4
		// assert suspendCount > 0;
		if (suspendCount < 1) {
			/** @nonissue **/
			throw new IllegalStateException("Thread is already resumed");
		}
		
		suspendCount--;
		notifyAll();
	}

	/**
	 * The thread function. This is a very simple method that polls for termination conditions and
	 * calls threadIterationGo and threadIterationWait.
	**/
	public final void run() {
		Logger.log(LogMessageCategory.INFO,"Thread starting for "+getClass()+".");
		System.out.flush();
		synchronized (activeThreads) {
			activeThreads.add(this);
			activeThreads.notifyAll();
		}

		try {
			if (startupInThread()) {
				while (!isSignaledToExit()) {
					try {
						if (!threadIterationGo()) {
							break;
						}

						threadIterationWait();
					} catch (InterruptedException exception) {

					}

					try {
						waitForPause();
					} catch (InterruptedException exception) {

					}
				}
			}
		} catch (Exception e) {
			incrementExceptionCount();
			/**
			 * @issue Thread startup failed for [className].
			 * @explain MOVES is unable to begin an internal thread.
			**/
			Logger.logError(e,"Thread startup failed for "+getClass()+".");
		}

		try {
			/**
			 * @issue Thread shutdown starting for [className].
			 * @explain MOVES is beginning a clean shutdown of an internal thread.
			**/
			Logger.log(LogMessageCategory.INFO,"Thread shutdown starting for "+getClass()+".");
			System.out.flush();
			shutdownInThread();
		} catch (Exception e) {
			incrementExceptionCount();
			/**
			 * @issue Thread shutdown failed for [className].
			 * @explain MOVES was unable to cleanly stop an internal thread.
			**/
			Logger.logError(e,"Thread shutdown failed for "+getClass()+".");
		}

		synchronized (activeThreads) {
			activeThreads.remove(this);
			activeThreads.notifyAll();
		}
		Logger.log(LogMessageCategory.INFO,"Thread shutdown complete for "+getClass()+".");
		System.out.flush();
	}
	
	/**
	 * Waits for the thread to be resumed if it is currently paused. If not paused, this
	 * will simply return immediately.
	 * @throws InterruptedException This is thrown if the thread is interrupted.
	**/
	public final synchronized void waitForPause() throws InterruptedException {
		while (!isSignaledToExit() && (suspendCount > 0)) {
			wait();
		}
	}

	/**
	 * Increments the exception count for this thread and any ancestors.
	**/
	void incrementExceptionCount() {
		exceptionCount++;

		if(parent != null) {
			parent.incrementExceptionCount();
		}
	}

	/**
	 * This performs one-time startup activities prior to threaded operation.
	 * @return Should the thread continue to run
	**/
	abstract protected boolean startupInThread();

	/** This performs one-time shutdown activities after threaded operation. **/
	abstract protected void shutdownInThread();

	/**
	 * This performs one iteration of thread functionality. Upon return, the MOVESThread
	 * will automatically check for termination conditions
	 * @return Should the thread continue to run
	 * @throws InterruptedException This is typically thrown when a sleep() or wait() call
	 * is interrupted.
	**/
	abstract protected boolean threadIterationGo() throws InterruptedException;

	/**
	 * Called directly after threadIterationGo. Subclasses typically implement a simple sleep
	 * operation here.
	 * @throws InterruptedException This is typically thrown when a sleep() call is interrupted.
	**/
	abstract protected void threadIterationWait() throws InterruptedException;
}
