import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.Frame;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;


public class PlugInDialogSaveGE4XHeader extends JDialogScriptableBase {
    private ModelImage image;
    
    /**
     * Constructor for the dynamic instantiation and execution of the SaveHeaderText script action.
     */
    public PlugInDialogSaveGE4XHeader() {
        super();
    }

    /**
     * Constructor used to record the SaveTab script action line.
     * 
     * @param img The image to save the header of.
     */
    public PlugInDialogSaveGE4XHeader(Frame theParentFrame, final ModelImage img) {
        super(theParentFrame, false);
        
        image = img;
        
        init();
        
        callAlgorithm();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    private void init() {
        // TODO gui for output file
    }

    @Override
    protected void callAlgorithm() {
        FileInfoBase[] fileInfo = image.getFileInfo();
        
        String infoStr = fileInfo[0].getAboutInfo(image.getMatrix());

        File outFile = new File(fileInfo[0].getFileDirectory() + File.separator + FileUtility.stripExtension(fileInfo[0].getFileName()) + "_header_export.txt");
        FileWriter writer = null;
        try {
            writer = new FileWriter(outFile);
            writer.write(infoStr);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        insertScriptLine();
    }

    @Override
    protected void setGUIFromParams() {
        // TODO optional save location param
        // TODO optional slice selection param
        image = scriptParameters.retrieveInputImage();
    }

    @Override
    protected void storeParamsFromGUI() throws ParserException {
        // TODO optional save location param
        // TODO optional slice selection param
        scriptParameters.storeInputImage(image);
    }
}
