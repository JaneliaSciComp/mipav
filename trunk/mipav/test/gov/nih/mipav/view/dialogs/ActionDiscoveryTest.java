package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Vector;


public class ActionDiscoveryTest {
    protected static BufferedReader input = new BufferedReader(new InputStreamReader(System.in));

    public static void main(final String[] args) {
        // need to create the mipav VUI, which is required to do virtually anything else using mipav
        MipavUtil.setForceQuiet(true);
        final ViewUserInterface ui = ViewUserInterface.create();
        ui.setVisible(false);

        // test discovery of discoverable algos and utils
        System.out.println("ActionDiscovery classes:");
        final Vector<Class<ActionDiscovery>> classes = ViewUserInterface.getDiscoverableActionList();
        for (final Class<ActionDiscovery> c : classes) {
            System.out.println(c);
        }

        for (final Class<ActionDiscovery> c : classes) {
            ActionDiscovery dialog = null;
            try {
                dialog = classes.elementAt(0).newInstance();
            } catch (final InstantiationException e) {
                e.printStackTrace();
                System.exit(1);
            } catch (final IllegalAccessException e) {
                e.printStackTrace();
                System.exit(1);
            }

            if (dialog == null) {
                System.err.println("Unable to instantiate class: " + c);
                continue;
            }

            System.out.println("####################################################");
            System.out.println(dialog.getActionMetadata());

            // get the input and output parameters of the dialog (which are not filled with any values)
            final ParameterTable inputParams = dialog.createInputParameters();
            final ParameterTable outputParams = dialog.createOutputParameters();

            // System.out.println("Initial Input parameters:");
            // System.out.println(inputParams.convertToString());

            // System.out.println("Initial Output parameters:");
            // System.out.println(outputParams.convertToString());

            try {
                // prep the global script runner, which manages the image table
                final ScriptRunner scriptRunner = ScriptRunner.getReference();
                scriptRunner.setRunning(true);
                scriptRunner.setImageTable(new ImageVariableTable());

                // set the other parameters
                String val = null;
                int curImageNum = 1;
                for (final Parameter param : inputParams.getParameters()) {
                    if (param.getType() != Parameter.PARAM_EXTERNAL_IMAGE && param.getType() != Parameter.PARAM_IMAGE) {
                        val = ActionDiscoveryTest.promptForParameterValue(param);

                        if (val == null) {
                            System.err.println("No value entered for parameter: " + param.convertToString());
                        } else {
                            param.setValue(val);
                        }
                    } else {
                        // set the value of the input image parameter placeholder
                        param.setValue("$image" + curImageNum);

                        // open a pre-determined image from disk (do not open the file chooser or put the image into a
                        // frame)
                        final String inputImageFileName = "C:\\Users\\mccreedy\\Desktop\\images\\anon\\xml\\genormcorp2_cor_256x256x32.xml";
                        final ViewOpenFileUI openFile = new ViewOpenFileUI(false);
                        openFile.setPutInFrame(false);
                        final String inputImageName = openFile.open(inputImageFileName, false, null);

                        if (inputImageName == null) {
                            System.err.println("Could not open input image: " + inputImageFileName);
                            System.exit(1);
                        }

                        // testing output file retrieval - output image should be "genormcorp2_cor_256x256x32_gblur1"
                        /*
                         * final String inputImageName2 = openFile.open(inputImageFileName, false, null); final
                         * ModelImage img = ui.getRegisteredImageByName(inputImageName2);
                         * img.setImageName("genormcorp2_cor_256x256x32_gblur");
                         */

                        // map the name input image we have opened to the image placeholder ($image1)
                        scriptRunner.storeImage(inputImageName);

                        curImageNum++;
                    }
                }
            } catch (final ParserException e) {
                e.printStackTrace();
            }

            System.out.println("Pre-execution Input parameters:");
            System.out.println(inputParams.convertToString());

            // run the script with the parameters we have set up
            dialog.scriptRun(inputParams);

            // pull the actual image name of the action's result image
            final String outputImageName = dialog.getOutputImageName(outputParams.getParameters()[0].getLabel());

            System.out.println("Post-exectuion Output image name:");
            System.out.println(outputImageName);

            // show the result image
            new ViewJFrameImage(ui.getRegisteredImageByName(outputImageName));
        }

        // System.exit(0);
    }

    protected static String promptForParameterValue(final Parameter param) {
        String val = null;

        System.out.print("Enter value for parameter: " + param.getLabel() + " (" + param.getTypeString() + ") = ");

        try {
            switch (param.getType()) {
                case Parameter.PARAM_BOOLEAN:
                case Parameter.PARAM_DOUBLE:
                case Parameter.PARAM_FLOAT:
                case Parameter.PARAM_INT:
                case Parameter.PARAM_LONG:
                case Parameter.PARAM_SHORT:
                case Parameter.PARAM_STRING:
                case Parameter.PARAM_USHORT:
                case Parameter.PARAM_LIST:
                    val = ActionDiscoveryTest.input.readLine();
                    break;
                case Parameter.PARAM_EXTERNAL_IMAGE:
                case Parameter.PARAM_IMAGE:
                    break;
                default:
                    System.err.println("Unrecognized parameter type: " + param.getTypeString());
            }
        } catch (final IOException e) {
            e.printStackTrace();
        }

        return val;
    }
}
