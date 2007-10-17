package gov.nih.mipav.model.scripting.actions;


import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;

import gov.nih.mipav.view.Preferences;


/**
 * A base class for all non-algorithmic (not JDialog*) script actions.
 */
public abstract class ActionBase implements ScriptableActionInterface {
	
	protected boolean isScript = true;
	
    /**
     * {@inheritDoc}
     */
    public abstract void insertScriptLine();
    
    /**
     * {@inheritDoc}
     */
    public abstract void scriptRun(ParameterTable parameters);
    
    /**
     * Returns the script action command string for this action.
     * 
     * @return  The script command which should be used for this action (e.g., Clone for ActionClone).
     */
    public String getActionName() {
        return ActionBase.getActionName(this.getClass());
    }
    
    /**
     * Returns the script command string for an action.
     * 
     * @param   actionClass  The class to find the script command string for.
     * 
     * @return  The script command which should be used for the given class (e.g., Clone for ActionClone).
     */
    public static final String getActionName(Class actionClass) {
        String name = actionClass.getName();
        
        int index = name.lastIndexOf("Action");
        
        if (index == -1) {
            // TODO: may be an fatal error..
            Preferences.debug("action base: No script Action prefix found.  Returning " + name + "\n", Preferences.DEBUG_SCRIPTING);
            return name;
        } else {
            Preferences.debug("action base: Extracting script action command.  Returning " + name.substring(index + 6) + "\n", Preferences.DEBUG_SCRIPTING);
            return name.substring(index + 6);
        }
    }
    
    public void setIsScript(boolean isScript) {
    	this.isScript = isScript;
    }
    
    public boolean isScript() { return this.isScript; }
}
