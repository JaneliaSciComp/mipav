package gov.nih.mipav.view;

import gov.nih.mipav.model.scripting.actions.ActionSaveBase;

/**
 * A MIPAV command line parser reads the arguments provided to MIPAV at the command line.  The parser
 * is expected to keep an internal cursor to represent the location of the next argument to be parsed.
 * The <code>parseArguments</code> function performs this functionality by receiving the array of arguments
 * and the location of the cursor.  When <code>parseArguments</code> returns <code>args.length</code>, this implies that
 * all arguments have been processed.
 * 
 * @author senseneyj
 *
 */
public interface CommandLineParser {

    /**
     * The <code>parseArguments</code> method parses command line arguments.  An internal cursor can be used to 
     * track the next argument to be processed.  The purpose of <code>initArg</code> is to tell this method
     * which argument to start processing.  This method returns the location of the next argument to be processed.
     * If this method returns an integer equal to <code>args.length</code>, then it is implied that all arguments
     * have been processed.  Note that the cursor does not necessarily need to return a value greater
     * than <code>initArg</code>. 
     * 
     * @param args command arguments
     * @param initArgs the location of the next command to be processed
     * 
     * @return the location of the next command to be processed, if equal to args.length, then no further processing is necessary
     */
    public int parseArguments(final String[] args, final int initArg);
}
