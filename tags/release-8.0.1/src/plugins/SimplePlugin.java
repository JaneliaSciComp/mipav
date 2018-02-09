import gov.nih.mipav.plugins.PlugInGeneric;
import gov.nih.mipav.view.CommandLineParser;

/**
 * Simple plugin for testing command line mipav
 * 
 * @author "Evan McCreedy"
 *
 */
public class SimplePlugin implements PlugInGeneric, CommandLineParser {

    public SimplePlugin() {
        System.out.println("Constructor calling");
    }
    
    @Override
    public int parseArguments(String[] args, int initArg) {
        System.out.println("Command line parsing");
        return args.length - 1;
    }

    @Override
    public void run() {
        // TODO Auto-generated method stub
        System.out.println("Plugin running");
    }

}
