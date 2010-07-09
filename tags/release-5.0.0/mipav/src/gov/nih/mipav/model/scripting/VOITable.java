package gov.nih.mipav.model.scripting;


import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;

import java.io.IOException;
import java.util.Hashtable;
import java.util.Vector;


/**
 * A table which manages paths to VOIs which should be used for specific images used within a script.
 *
 * @author mccreedy
 */
public class VOITable {
    /** The table containing vectors of VOI path strings, keyed by the image placeholder variables used in the script. */
    private Hashtable voiPathTable;
    
    /** A table containing counts of the number of VOIs which have been retrieved and registered for each image, keyed by the image placeholder variables used in the script. */
    private Hashtable voiPathRequestCountTable;
    
    /**
     * Create a new VOI path table.
     * 
     * @param   scriptFile   The script file being processed.
     * @param   voiPathList  A list of VOI paths, in the order returned by Parser.getImageVarsUsedInScript().
     * 
     * @throws  ParserException  If an error is encountered while retrieving the image vars used in the script.
     */
    public VOITable(String scriptFile, Vector voiPathList) throws ParserException {
        String[] imageVars = Parser.getImageVarsUsedInScript(scriptFile);
        
        voiPathTable = new Hashtable();
        voiPathRequestCountTable = new Hashtable();
        
        int currentVOIPathIndex = 0;
        for (int i = 0; i < imageVars.length; i++) {
            Vector imageVOIPaths = new Vector();
            int numVOIsRequired = Parser.getNumberOfVOIsRequiredForImageVar(scriptFile, imageVars[i]);
            int numVOIsFound = voiPathList.size() - currentVOIPathIndex;
            
            if (numVOIsRequired > numVOIsFound) {
                throw new ParserException(scriptFile, 0, "This script requires " + numVOIsRequired + " for image " + imageVars[i] + ".  Found " + numVOIsFound + ".");
            }
            
            for (int j = 0; j < numVOIsRequired; j++) {
                imageVOIPaths.addElement(voiPathList.elementAt(currentVOIPathIndex));
                currentVOIPathIndex++;
            }
            
            voiPathTable.put(imageVars[i], imageVOIPaths);
            voiPathRequestCountTable.put(imageVars[i], new Integer(0));
        }
    }
    
    /**
     * Opens the next VOI for a given image from disk, then registers that VOI in the appropriate image.
     * 
     * @param   imageVar  The image variable placeholder for whom to find the next VOI.
     * 
     * @throws  IOException  If there is a problem reading in the VOI file from disk.
     */
    public void openAndRegisterNextVOI(String imageVar) throws IOException {
        String voiPath = getNextVOIPath(imageVar);
        String voiDirectory = FileUtility.getFileDirectory(voiPath);
        String voiFileName = FileUtility.getFileName(voiPath);
        ModelImage image = ScriptRunner.getReference().getImage(imageVar);
   		
        //if the directory is null, this VOI is already loaded
        if (voiDirectory == null) {
        	return;
        }
        
        FileVOI fileVOI = new FileVOI(voiFileName, voiDirectory, image);
        VOI[] voi = fileVOI.readVOI(false);
        for (int i = 0; i < voi.length; i++) {
            image.registerVOI(voi[i]);
        }
    }
    
    /**
     * Returns the path to the next VOI which should be opened for a given image.
     * 
     * @param   imageVar  The image variable placeholder for whom to find the next VOI path.
     * 
     * @return  A path to a VOI file on disk.
     */
    private String getNextVOIPath(String imageVar) {
        int currentVOIIndex = ((Integer)voiPathRequestCountTable.get(imageVar)).intValue();
        Vector imageVOIPaths = (Vector)voiPathTable.get(imageVar);
        
        String voiPath = (String)imageVOIPaths.elementAt(currentVOIIndex);
        incrementVOIRequestCount(imageVar);
        return voiPath;
    }
    
    /**
     * Increments the count of VOI-file-open requests for a given image in the script.
     * 
     * @param  imageVar  The image variable placeholder for which we have just processed a VOI-file-open request.
     */
    private void incrementVOIRequestCount(String imageVar) {
        int oldValue = ((Integer)voiPathRequestCountTable.get(imageVar)).intValue();
        voiPathRequestCountTable.remove(imageVar);
        voiPathRequestCountTable.put(imageVar, new Integer(oldValue + 1));
    }
}
