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
    
    public interface Command {
        /**Returns help for using command. */
        public String getHelp();
        
        /** Returns command in lower-case form. */
        public String getCommand();
        
        /**Identifies command that will be used. */
        public Command getCommand(String str);
    }
    
    public enum InstanceCommand implements Command {
        Image("i", "Image file name"),
        MultiImage("m", "Image multifile name"),
        RawImage("r", "Raw Image Info (example: -r datatype;extents;resols;units;endian;offset)\n"
                        + "\t\tSupported raw image file data types:\n"
                            + "\t\t\t0 => Boolean\n"
                            + "\t\t\t1 => Byte\n"
                            + "\t\t\t2 => Unsigned Byte\n"
                            + "\t\t\t3 => Short\n"
                            + "\t\t\t4 => Unsigned Short\n"
                            + "\t\t\t5 => Integer\n"
                            + "\t\t\t6 => Long\n"
                            + "\t\t\t7 => Float\n"
                            + "\t\t\t8 => Double\n"
                            + "\t\t\t9 => ARGB\n"
                            + "\t\t\t10 => ARGB Unsigned Short\n"
                            + "\t\t\t11 => ARGB Float\n"
                            + "\t\t\t12 => Complex\n"
                            + "\t\t\t13 => Complex Double\n"
                            + "\t\t\t14 => Unsigned Integer"
                            +"\t\tThe extents, resolutions and units for each image dimension should be separated by commas.\n"
                            +"\t\tFor endianess, true => big endian and false => little endian."),
        Hide("hide", "Hide application frame"),
        Script("s", "Script file name"),
        Voi("v", "VOI file name"),
        SavedImageName("o", "Saved image file name (sets "+ ActionSaveBase.SAVE_FILE_NAME+ " parameter)"),
        ScriptVariable("d", "Set the value of a variable used in a script"),
        Plugin("p", "Run a plugin");
        
        /**Help for using a command. */
        private String help;
        /** The case-independent form of the command */
        private String command;
        /** Alternate commands for a given action */
        private String[] altCommand;
        
        InstanceCommand(String command, String help, String... altCommand) {
            this.command = command;
            this.help = help;
            this.altCommand = altCommand;
        }

        public String getHelp() {
            return help;
        }

        public String getCommand() {
            return command;
        }
        
        public String toString() {
            return command;
        }

        public Command getCommand(String str) {
            InstanceCommand cmd = null;
            str = str.toLowerCase();
            if(str.length() > 0 && str.charAt(0) == '-') {
                str = str.substring(1);
            }
            if((cmd = InstanceCommand.valueOf(str)) != null) {
                return cmd;
            } else {
                for(InstanceCommand c : InstanceCommand.values()) {
                    for(int i=0; i<c.altCommand.length; i++) {
                        if(str.equals(altCommand[i])) {
                            return c;
                        }
                    }
                }
                
                Preferences.debug("No matching instance command found for "+cmd);
                return null;
            }
        }
        
        
    }
    
    public enum StaticCommand implements Command {
        HomeDir("homedir", "Set the mipav home directory"),
        PreferencesDir("prefdir", "Mipav preferences directory (defaults to home directory"),
        PreferencesName("prefname", "Name of the mipav preferences file"),
        InputDir("inputdir", "Image input directory"),
        OutputDir("outputdir", "Image output directory"),
        PluginDir("plugindir", "Plugin directory"),
        Help("h", "Display this help", "help", "usage");
        
        /**Help for using a command. */
        private String help;
        /** The case-independent form of the command */
        private String command;
        /** Alternate commands for a given action */
        private String[] altCommand;
        
        StaticCommand(String s, String help, String... altCommand) {
            this.command = s;
            this.help = help;
            this.altCommand = altCommand;
        }
        
        StaticCommand(String[] s, String help) {
            
        }

        public String getHelp() {
            return help;
        }

        public String getCommand() {
            return command;
        }
        
        public String toString() {
            return command;
        }

        public Command getCommand(String str) {
            StaticCommand cmd = null;
            str = str.toLowerCase();
            if(str.length() > 0 && str.charAt(0) == '-') {
                str = str.substring(1);
            }
            if((cmd = StaticCommand.valueOf(str)) != null) {
                return cmd;
            } else {
                for(StaticCommand c : StaticCommand.values()) {
                    for(int i=0; i<c.altCommand.length; i++) {
                        if(str.equals(altCommand[i])) {
                            return c;
                        }
                    }
                }
                
                Preferences.debug("No matching instance command found for "+cmd);
                return null;
            }
        }
    }
    
    
}
