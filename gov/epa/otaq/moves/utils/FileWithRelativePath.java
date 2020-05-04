package gov.epa.otaq.moves.utils;

import java.io.File;

public class FileWithRelativePath {
	private File file;
	private String relativPath = "";
	
	public FileWithRelativePath(File file, String relativPath)
	{
		this.setFile(file);
		this.setRelativPath(relativPath);
	}

	public File getFile() {
		return file;
	}

	public void setFile(File file) {
		this.file = file;
	}

	public String getRelativPath() {
		return relativPath;
	}

	public void setRelativPath(String relativPath) {
		this.relativPath = relativPath;
	}
	
}
