package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.scripting.parameters.Parameter;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;

import gov.nih.mipav.view.Preferences;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;

import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Performs the heavy-lifting of parsing out individual lines from a script.
 */
public class ParserEngine {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** This regular expression pattern breaks apart a line in a script into two groups: the script action and the string containing all the action's parameters. */
    private static final Pattern scriptLineRegex = Pattern.compile("^([A-Za-z][\\w_.]*)\\s*\\(\\s*(\".+\")*\\s*\\)\\s*$");

    /** This regular expression pattern breaks apart an action parameter string into three parts: param label, param data type, and param data. */
    private static final Pattern paramInfoRegex = Pattern.compile("^([$\\w_.-]+)\\s+([\\w_]+)\\s+(.*)$");

    /** The delimiter used to split up a String containing all of the parameters which should be parsed and passed to an action. */
    private static final String paramListDelimiter = "\",\\s*\"";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The number of the current (or last if not currently parsing) line being parsed (1-based). */
    private int currentScriptLineNumber = 0;

    /** The path to the script file to be parsed ('-' if the script is already in memory and has no related file). */
    private String scriptFile;

    /** The reader used to read in each line of the script. */
    private BufferedReader scriptReader;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ParserEngine object.
     *
     * @param   script               Either the path to the script on disk, or the text of the script. 
     * @param   scriptIsFileToParse  If <code>true</code>, then <code>script</code> is the path to a file on disk.  Otherwise, it is the text of a script and no file should be read in from disk.
     *
     * @throws  ParserException  If a problem is encountered while parsing the script.
     */
    public ParserEngine(String script, boolean scriptIsFileToParse) throws ParserException {

        if (scriptIsFileToParse) {
            scriptFile = script;
            scriptReader = openScriptFile(script);
        } else {
            scriptFile = "-";
            scriptReader = openScriptString(script);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean up memory used by the parser engine and make sure the reader we used is closed.
     */
    public void finalize() {

        if (scriptReader != null) {

            try {
                scriptReader.close();
            } catch (IOException ioe) {
                Preferences.debug("parserEng:\tError encountered trying to close script reader: " + scriptFile + "\n", Preferences.DEBUG_SCRIPTING);
            }
        }
    }

    /**
     * Returns the line number of the last line parsed by the parser engine.
     *
     * @return  The line number of the last line parsed by the parser engine.
     */
    public int getCurrentLineNumber() {
        return currentScriptLineNumber;
    }

    /**
     * Returns whether there are more lines in the script to be parsed.
     *
     * @return  <code>True</code> if the parser engine has not reached the end of the script.
     *
     * @throws  ParserException  If a problem is encountered while parsing the script.
     */
    public boolean hasMoreLinesToParse() throws ParserException {

        try {
            return scriptReader.ready();
        } catch (IOException ioe) {
            throw new ParserException(scriptFile, currentScriptLineNumber, ioe.getMessage());
        }
    }

    /**
     * Parse and return the next line in the script.
     *
     * @return  The next line in the script, parsed into action and parameter table.
     *
     * @throws  ParserException  If a problem is encountered while parsing the script.
     */
    public ParsedActionLine parseNextLine() throws ParserException {
        currentScriptLineNumber++;

        ParsedActionLine parsedLine = null;

        try {
            String str = null;

            if ((str = scriptReader.readLine()) != null) {
                Preferences.debug("parserEng:\tParsing script line number " + currentScriptLineNumber + "\n", Preferences.DEBUG_SCRIPTING);
                str = str.trim();

                if (!str.equals("")) {
                    parsedLine = parseLine(str);
                } else {
                    Preferences.debug("parserEng:\t\tSkipping empty line.\n", Preferences.DEBUG_SCRIPTING);
                }
            }
        } catch (IOException ioe) {
            throw new ParserException(scriptFile, currentScriptLineNumber, ioe.getMessage());
        } catch (ParserException pe) {
            pe.setParsedFileName(scriptFile);
            pe.setLineNumber(currentScriptLineNumber);
            throw pe;
        }

        return parsedLine;
    }

    /**
     * Resets the parser to the beginning of the script.
     * 
     * @throws  ParserException  If a problem is encountered while resetting the script parser.
     */
    public void resetParser() throws ParserException {
        currentScriptLineNumber = 0;
        try {
            scriptReader.reset();
        } catch (IOException ioe) {
            throw new ParserException(scriptFile, currentScriptLineNumber, "Error resetting parser: " + ioe.getMessage());
        }
    }

    /**
     * Checks to see if a line is a comment.
     *
     * @param   scriptLine  A line from a script.
     *
     * @return  <code>True</code> if the line is a comment, <code>false</code> otherwise.
     */
    private static boolean isCommentLine(String scriptLine) {

        if (scriptLine.startsWith("#") || scriptLine.startsWith("//") ||
                scriptLine.toLowerCase().startsWith("comment")) {
            return true;
        }

        return false;
    }

    /**
     * Open a script file and prepare a reader for it.
     *
     * @param   scriptFileToParse  Path on disk to a script file to open.
     *
     * @return  A reader for the script file.
     *
     * @throws  ParserException  If a problem is encountered while parsing the script.
     */
    private BufferedReader openScriptFile(String scriptFileToParse) throws ParserException {

        try {
            return new BufferedReader(new FileReader(scriptFileToParse));
        } catch (FileNotFoundException fnfe) {
            throw new ParserException(scriptFileToParse, 0, fnfe.getMessage());
        }
    }

    /**
     * Prepare a reader for a given string of script data. 
     *
     * @param   scriptDataToParse  The contents of a script.
     *
     * @return  A reader for the script contents.
     *
     * @throws  ParserException  If a problem is encountered while parsing the script.
     */
    private BufferedReader openScriptString(String scriptDataToParse) throws ParserException {
        return new BufferedReader(new StringReader(scriptDataToParse));
    }

    /**
     * Parses a line from a script.
     *
     * @param   scriptLine  A line of text from a script.
     *
     * @return  The parsed-out information from the script line.
     *
     * @throws  ParserException  If a problem is encountered while parsing the script.
     */
    private ParsedActionLine parseLine(String scriptLine) throws ParserException {

        // skip comment lines
        if (ParserEngine.isCommentLine(scriptLine)) {
            Preferences.debug("parserEng:\t\tSkipping comment line.\n", Preferences.DEBUG_SCRIPTING);

            return null;
        }

        Matcher matcher = scriptLineRegex.matcher(scriptLine);

        String actionName;
        String paramString;
        ParameterTable paramList;

        // need to catch when the line isn't in the correct format
        if (!matcher.find()) {
            throw new ParserException("Script lines must use the following format:\n ActionName(\"param_name param_type param_value\", ...)");
        }

        actionName = matcher.group(1);
        paramString = matcher.group(2);

        Preferences.debug("parserEng:\t\t" + actionName + " -- " + paramString + "\n", Preferences.DEBUG_SCRIPTING);

        paramList = parseParameters(paramString);

        return new ParsedActionLine(actionName, paramList);
    }

    /**
     * Parse out the individual parameter information from a string.
     *
     * @param   paramInfo  Parameter information string.
     *
     * @return  The new parameter.
     *
     * @throws  ParserException  If a problem is encountered while parsing the script.
     */
    private Parameter parseParameterInfo(String paramInfo) throws ParserException {
        Parameter param = null;

        Matcher matcher = paramInfoRegex.matcher(paramInfo);

        while (matcher.find()) {

            if (matcher.groupCount() == 3) {
                param = ParameterFactory.parseParameter(matcher.group(1), matcher.group(2), matcher.group(3));

                // Preferences.debug(param.convertToString(), Preferences.DEBUG_SCRIPTING);
            } else {
                throw new ParserException("Error parsing parameter: " + paramInfo);
            }
        }

        return param;
    }

    /**
     * Splits a list of parameters into individual parameter information strings, parses the individual parameter strings, and puts the parameters into a look up table.
     *
     * @param   paramString  A list of parameters.
     *
     * @return  A table containing all the parameters extracted from the string.
     *
     * @throws  ParserException  If a problem is encountered while parsing the script.
     */
    private ParameterTable parseParameters(String paramString) throws ParserException {
        ParameterTable paramList = new ParameterTable();

        if (paramString == null) {

            // no parameters for algorithm, return empty vector
            Preferences.debug("parserEng:\t\t\tNo parameters found.\n", Preferences.DEBUG_SCRIPTING);

            return paramList;
        }

        String[] params = paramString.split(paramListDelimiter);

        for (int i = 0; i < params.length; i++) {

            if (params[i].startsWith("\"")) {
                params[i] = params[i].substring(1);
            }

            if (params[i].endsWith("\"")) {
                params[i] = params[i].substring(0, params[i].length() - 1);
            }

            // Preferences.debug("\t\t" + params[i] + "\n", Preferences.DEBUG_SCRIPTING);

            Parameter param = parseParameterInfo(params[i]);
            paramList.put(param);
        }

        return paramList;
    }
}
