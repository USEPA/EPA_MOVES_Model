package gov.epa.otaq.moves.common;

/**
 * This class, which acts like a "typesafe enum", describes the model at which a
 * simulation can be run at. 
 * 
 * A model has a name and an id. The nth model's id 2**(n-1). This is to ensure 
 * that only the id's nth bit (from right) is one, while others are all zeros. 
 * By taking bitwise OR for all the chosen models's ids, we can easily to determine
 * what models are included, thus make the detection easier.
 * 
 * With the new feature ENUM from Java 1.5, this is not necessary anymore. But still 
 * keep this style to match other old code. And this can show for example, the use of
 * private constructors.
 *  
 * @author		Jizhen (Jason) Zhao at IE, UNC-CH
 * @version		$Revision$ $Date$
 * @since       2011-11-29
**/

public class Model {
	
	/** Array of model IDs **/
	private static final int [] MODEL_IDS = {
		1, 
		1<<1, 
		1<<2
		};
	
	/** Array of model names **/
	private static final String [] MODEL_NAMES = {
		"ONROAD",
		"NONROAD",
		"Model3"
		};

	/** The list of all Model instances **/
	static TreeMapIgnoreCase allInstances = new TreeMapIgnoreCase();

	/** Enumerated type (as static instance) for a model value. **/
	public static final Model ONROAD = new Model(MODEL_NAMES[0], MODEL_IDS[0]);
	
	/** Enumerated type (as static instance) for a model value. **/
	public static final Model NONROAD = new Model(MODEL_NAMES[1], MODEL_IDS[1]);
	
	/** Enumerated type (as static instance) for a model value. **/
	public static final Model MODEL3 = new Model(MODEL_NAMES[2], MODEL_IDS[2]);
	
	/** The textual description of the type. **/
	private String name;
	
	/** The integer id of the type **/
	private int id;

	/**
	 * Constructor, but private, and as such this class can not be instantiated except
	 * by the static types defined above.
	 * 
	 * @param name - Textual description of the type.
	**/
	private Model(String description, int id) {
		this.name = description;
		this.id = id;
		allInstances.put(description,this);
	}

	/**
	 * Provides the textual description of the type.
	 * 
	 * @return name - The textual description of the type.
	**/
	public String toString() {
		return name;
	}

	/**
	 * Gets the constant type based on a text name.
	 * 
	 * @param name The textual description of the type
	 * @return The constant type. This will be null if the given name wasn't found.
	**/
	public static Model getByName(String name) {
		return (Model)allInstances.get(name);
	}

	/**
	 * Gets the id of this type.
	 * 
	 * @return id - int.
	**/
	public int getId() {
		return id;
	}

}

