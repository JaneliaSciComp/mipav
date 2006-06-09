package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.scripting.parameters.Parameter;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.util.Vector;


/**
 * This is the main class used to parse and run scripts.  It may also be used to retrieve the image placeholder
 * variables (e.g., $image1) used in a script and the number of VOIs required for a particular image placeholder.
 */
public class Parser {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns an array of image placeholder variables used in a given script.
     *
     * @param   scriptFile  The full path to the script file on disk which should be parsed.
     *
     * @return  An array of the image placeholder variables (e.g. $image1) used in <code>scriptFile</code>.
     *
     * @throws  ParserException  If a problem is encountered while parsing the script.
     */
    public static final String[] getImageVarsUsedInScript(String scriptFile) throws ParserException {
        Vector imageVarList = new Vector();
        int numImages = 0;
        ParserEngine parser = new ParserEngine(scriptFile, true);

        while (parser.hasMoreLinesToParse()) {
            ParsedActionLine parsedLine = parser.parseNextLine();

            if (parsedLine != null) {
                Parameter[] params = parsedLine.getParameterTable().getParameters();

                for (int i = 0; i < params.length; i++) {

                    if (params[i].getType() == Parameter.PARAM_IMAGE) {

                        if (!imageVarList.contains(params[i].getValueString())) {
                            imageVarList.add(params[i].getValueString());
                            numImages++;
                        }
                    }
                }
            }
        }

        String[] imageVars = new String[imageVarList.size()];

        for (int i = 0; i < imageVars.length; i++) {
            imageVars[i] = (String) imageVarList.elementAt(i);
        }

        Preferences.debug("Found " + numImages + " unique images used in script " + scriptFile,
                          Preferences.DEBUG_MINOR);

        return imageVars;
    }

    /**
     * Returns the number of VOIs which will be opened for a given image (<code>imageVarName</code>) when a particular script is run.
     *
     * @param   scriptFile    The full path to the script file on disk which should be parsed.
     * @param   imageVarName  The image placeholder variable (e.g. $image1) for which to count required VOIs.
     *
     * @return  The number of VOIs required for the given <code>imageVarName</code>.
     *
     * @throws  ParserException  If a problem is encountered while parsing the script.
     */
    public static final int getNumberOfVOIsRequiredForImageVar(String scriptFile, String imageVarName)
            throws ParserException {
        int numVOIs = 0;
        ParserEngine parser = new ParserEngine(scriptFile, true);

        while (parser.hasMoreLinesToParse()) {
            ParsedActionLine parsedLine = parser.parseNextLine();

            if (parsedLine != null) {

                if (Parser.isOpenVOIAction(parsedLine.getAction())) {

                    if (imageVarName.equalsIgnoreCase(Parser.getVOIParentImage(parsedLine))) {
                        numVOIs++;
                    }
                }
            }
        }

        Preferences.debug("Found " + numVOIs + " unique VOIs used in script " + scriptFile, Preferences.DEBUG_MINOR);

        return numVOIs;
    }

    /**
     * Testing main method.
     * TODO: remove this method once testing is completed.
     *
     * @param  args  path to a script to parse and run.
     */
    public static final void main(String[] args) {

        try {
            Parser.runScript(args[0]);
        } catch (ParserException pe) {
            MipavUtil.displayError("Fatal error encountered running script:\n" + pe);
        }
    }

    /**
     * Parses and runs the commands contained within a given script file.
     *
     * @param   scriptFile  The full path to the script file on disk which should be parsed.
     *
     * @throws  ParserException  If a problem is encountered while parsing the script.
     */
    public static final void runScript(String scriptFile) throws ParserException {
        ParserEngine parser = new ParserEngine(scriptFile, true);

        while (parser.hasMoreLinesToParse()) {
            ParsedActionLine parsedLine = parser.parseNextLine();

            if (parsedLine != null) {
                Preferences.debug("\t" + parsedLine.convertToString(), Preferences.DEBUG_MINOR);

                ScriptableActionInterface scriptAction = ScriptableActionLoader.getScriptableAction(parsedLine.getAction());
                scriptAction.scriptRun(parsedLine.getParameterTable());
            }
        }
    }

    /**
     * Returns the image placeholder variable referenced by an 'OpenVOI' action.
     *
     * @param   parsedLine  A parsed line from a script (should be an 'OpenVOI' action).
     *
     * @return  The image placeholder variable for the image the VOI will be loaded into. 
     */
    private static String getVOIParentImage(ParsedActionLine parsedLine) {

        // TODO: it is not the best policy to hard code here that the OpenVOI key for the attached image is
        // "parent_image"...
        return parsedLine.getParameterTable().get("parent_image").getValueString();
    }

    /**
     * Returns whether a action string would result in the opening of a new image.
     *
     * @param   action  The action string to check.
     *
     * @return  Whether the given action will open a new image.
     */
    private static boolean isOpenImageAction(String action) {

        if (action.equalsIgnoreCase("OpenImage") || action.equalsIgnoreCase("OpenMultiFile") ||
                action.equalsIgnoreCase("LoadImage")) {
            return true;
        }

        return false;
    }

    /**
     * Returns whether a action string would result in the opening of a VOI.
     *
     * @param   action  The action string to check.
     *
     * @return  Whether the given action will open a VOI.
     */
    private static boolean isOpenVOIAction(String action) {

        if (action.equalsIgnoreCase("OpenVOI")) {
            return true;
        }

        return false;
    }
}
