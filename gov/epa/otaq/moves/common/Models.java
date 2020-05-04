package gov.epa.otaq.moves.common;

import java.util.ArrayList;
import java.util.List;

/**
 * This class, is a container for Model object. It also provides a ENUM for model 
 * combinations and a static method to evaluate the model combinations, since we
 * need to check what is the combination of the chosen models, in addition to the
 * individual models, when multiple models allowed to be chosen.
 *  
 * @author		Jizhen (Jason) Zhao at IE, UNC-CH
 * @version		$Revision$ $Date$
 * @since       2013-06-12
**/
public class Models {
	/** Enumerate model combinations. M0 means no model, M1 means only model 1, M123 means models 1, 2 and 3 exist. **/
	public enum ModelCombination {
		M0, M1, M2, M12, M3, M13, M23, M123;
		public static ModelCombination get(int i) {
			return values()[i];
		}
	};
	
	/** The list to hold the Model objects. */
	private List<Model> modelList;
	
	/**
	 * Default constructor. Create a new Model list.
	**/
	public Models() {
		modelList = new ArrayList<Model>();
	}

	/**
	 * Constructor for a default model.
	 * @param model default model to be used.
	**/
	public Models(Model model) {
		modelList = new ArrayList<Model>();
		modelList.add(model);
	}
	
	/**
	 * Copy constructor. 
	 * @param models of type Models
	**/
	public Models( Models models) {
		this.modelList = models.getModelList();
	}
	
	/**
	 * Utility method to evaluate what Model combination in the models of type Models. 
	 * @param models of type Models
	**/
	public static final ModelCombination evaluateModels( Models models) {
		if (models == null) {
			return ModelCombination.M0;
		}
		int result = 0;
		for ( Model model : models.getModelList()) {
			result |= model.getId();
		}
		return ModelCombination.get(result);
	}

	/**
	 * Check the list of models for a specific model.
	 * @param model model to be searched
	 * @return true if model is within the model list
	**/
	public boolean contains(Model model) {
		for(Model m : modelList) {
			if(m.getId() == model.getId()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Check for any overlap between the list of models
	 * and those in a ModelCombination.
	 * @param mc flags indicating a combination of models
	 * @return true if any model in the input is present
	 * in the list of models.
	**/
	public boolean containsAny(ModelCombination mc) {
		int existingBits = 0;
		for(Model m : modelList) {
			existingBits |= m.getId();
		}
		return (existingBits & mc.ordinal()) > 0;
	}

	/**
	 * Getter for field modelList.
	 * @return modelList of List<Model>.
	 */
	public List<Model> getModelList() {
		return modelList;
	}
	
	/**
	 * Getter for field modelList.
	 * @return modelList of List<Model>.
	 */
	public void clear() {
		modelList.clear();
	}

	/**
	 * Setter for field modelList.
	 * @param modelList of List<Model>
	 */
	public void setModelList(List<Model> modelList) {
		this.modelList = modelList;
	}
	
	/**
	 * A convenient method - Get the model at position index.
	 * @param index of int
	 * @return Model
	 */
	public Model get(int index) {
		return modelList.get(index);
	}
	
	/**
	 * A convenient method - add a model to the container.
	 * @param model of type Model
	 */
	public void add(Model model) {
		if (model==null) {
			return;
		}
		if(!modelList.contains(model)) {
			modelList.add(model);
		}
	}
	
	/**
	 * Print model combination in a Models object.
	 */
	private static void printCombination(final Models models) {
		ModelCombination com = Models.evaluateModels(models);
		switch (com) {
		case M0:
			System.out.println("Model 1   2   3");
			System.out.println("Exist 0   0   0");
			break;
		case M1:
			System.out.println("Model 1   2   3");
			System.out.println("Exist x   0   0");
			break;
		case M2:
			System.out.println("Model 1   2   3");
			System.out.println("Exist 0   x   0");
			break;
		case M3:
			System.out.println("Model 1   2   3");
			System.out.println("Exist 0   0   x");
			break;
		case M12:
			System.out.println("Model 1   2   3");
			System.out.println("Exist x   x   0");
			break;
		case M13:
			System.out.println("Model 1   2   3");
			System.out.println("Exist x   0   x");
			break;
		case M23:
			System.out.println("Model 1   2   3");
			System.out.println("Exist 0   x   x");
			break;
		case M123:
			System.out.println("Model 1   2   3");
			System.out.println("Exist x   x   x");
		}
		System.out.println();
	}
	/** 
	 * Main - simple test method
	 *
	 */
	public static void main( String [] args) {
		Models models = new Models();
		Models.printCombination(models);
		models.add(Model.getByName("ONROAD"));
		Models.printCombination(models);
		models.add(Model.getByName("NONROAD"));
		Models.printCombination(models);
		models.add(Model.getByName("MODEL3"));
		Models.printCombination(models);
		
		models = new Models();
		models.add(Model.getByName("ONROAD"));
		models.add(Model.getByName("MODEL3"));
		Models.printCombination(models);
		
		models = new Models();
		models.add(Model.getByName("NONROAD"));
		models.add(Model.getByName("MODEL3"));
		Models.printCombination(models);
		
		models = new Models();
		models.add(Model.getByName("MODEL3"));
		Models.printCombination(models);
		
		models = new Models(models);
		Models.printCombination(models);
		
		models = new Models();
		models.add(Model.getByName("MODEL_NOT_Exist"));
		Models.printCombination(models);
	}
}
