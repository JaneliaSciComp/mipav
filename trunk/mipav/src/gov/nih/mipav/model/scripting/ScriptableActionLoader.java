package gov.nih.mipav.model.scripting;


import gov.nih.mipav.view.Preferences;

import java.util.Vector;


/**
 * Given the name of a script action, this class searches a number of locations for a class to load with that name and
 * returns a new instance of that class.
 */
public class ScriptableActionLoader {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Try MIPAV dialogs, then special script actions, then plugins (which are in the default package). */
    private static Vector<String> SCRIPT_ACTION_LOCATIONS = new Vector<String>();

    static {
        ScriptableActionLoader.addScriptActionLocation("gov.nih.mipav.view.dialogs.JDialog");
        ScriptableActionLoader.addScriptActionLocation("gov.nih.mipav.model.scripting.actions.Action");
    };

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Returns a new instance of the class associated with a given script action.
     * 
     * @param action The action to search for a class of.
     * 
     * @return A new instance of a script action's class.
     * 
     * @throws ParserException If an associated class was not found or could not be loaded.
     */
    public static final ScriptableActionInterface getScriptableAction(final String action) throws ParserException {

        try {
            return (ScriptableActionInterface) ScriptableActionLoader.loadClass(action).newInstance();
        } catch (final IllegalAccessException err) {
            throw new ParserException("Access denied -- " + err.getMessage());
        } catch (final InstantiationException err) {
            throw new ParserException("Unable to instantiate class -- " + err.getMessage()
                    + ". An empty constructor is likely needed.");
        }
    }

    /**
     * Trys to load a action's class, searching in a specific package.
     * 
     * @param packageString The prefix to append to the action before trying to load it (may be a package, a package and
     *            a class name prefix, or empty for the default package).
     * @param action The script action to search for.
     * 
     * @return The action's corresponding class, from the given package prefix.
     * 
     * @throws ParserException If the action's class could not be found or loaded from the given package prefix.
     */
    protected static Class attemptLoadFromPackage(final String packageString, final String action)
            throws ParserException {

        try {
            Preferences.debug("script action loader:\tTrying action:\t" + action + "\tin\t" + packageString + "\n",
                    Preferences.DEBUG_SCRIPTING);
            return Class.forName(packageString + action);
        } catch (final ClassCastException err) {

            // plugin does not implement scriptable interface
            throw new ParserException(packageString + action
                    + " does not allow itself to be scripted.  See ScriptableActionInterface.");
        } catch (final Exception err) {
        	if(action.contains(".")) {
        		try {
					return Class.forName(action);
				} catch (ClassNotFoundException e) {
					//fall through to ParserException
				}
        	}
            // can't load class
            throw new ParserException("Cannot load script action: " + packageString + action);
        }
    }

    /**
     * Trys to find and load an action's corresponding class from a list of package prefixes. The first place where a
     * suitable class is found is the one returned.
     * 
     * @param action The script action to search for.
     * 
     * @return The action's corresponding class, found using one of the package prefixes.
     * 
     * @throws ParserException If the action's class could not be found or loaded from any of the script action
     *             locations.
     */
    protected static Class loadClass(final String action) throws ParserException {

        for (int i = 0; i < ScriptableActionLoader.SCRIPT_ACTION_LOCATIONS.size(); i++) {

            try {
                return ScriptableActionLoader.attemptLoadFromPackage(ScriptableActionLoader.SCRIPT_ACTION_LOCATIONS
                        .get(i), action);
            } catch (final ParserException pe) {
                Preferences.debug("script action loader:\tAction not found in package:\t" + action + "\tin\t"
                        + ScriptableActionLoader.SCRIPT_ACTION_LOCATIONS.get(i) + "\n", Preferences.DEBUG_SCRIPTING);
            }
        }

        // try the default package
        try {
            return ScriptableActionLoader.attemptLoadFromPackage("", action);
        } catch (final ParserException pe) {
            Preferences.debug("script action loader:\tAction not found in package:\t" + action + "\tin\t"
                    + " <default package> " + "\n", Preferences.DEBUG_SCRIPTING);

            throw pe;
        }
    }

    /**
     * Adds a new package where we should look for scriptable actions. To be used to register the package paths used by
     * more complex plugins (probably called from the plugin run() method).
     * 
     * @param packageString The java package in which to look for scriptable actions.
     */
    public static void addScriptActionLocation(final String packageString) {
        if ( !ScriptableActionLoader.SCRIPT_ACTION_LOCATIONS.contains(packageString)) {
            ScriptableActionLoader.SCRIPT_ACTION_LOCATIONS.add(packageString);
        }
    }

    /**
     * Gets the script action location vector, needed for figuring out the possible short name of a non-MIPAV scriptable
     * tool.
     */
    public static Vector<String> getScriptActionLocations() {
        return ScriptableActionLoader.SCRIPT_ACTION_LOCATIONS;
    }
}
