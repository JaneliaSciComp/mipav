package gov.nih.mipav.model.file;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.*;

import java.io.File;
import java.io.IOException;

import javax.swing.JFileChooser;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import gov.nih.mipav.model.file.xcede.DocumentFactory;
import gov.nih.mipav.model.file.xcede.XMLFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.*;

/**
 * A memory representation of the XCEDE file, and in charge of open
 * and save of the xcede file.
 * 
 * @author Hailong Wang, Ph.D
 * @version 1.0, 05/20/2006.
 */
public class FileXCEDE implements DocumentFactory, XMLFactory {
    public static String[] dataTypes = { "int8", "uint8", "int16", "uint16",
            "int32", "uint32", "int64", "uint64", "float32", "float64", "ascii" };

    public static int[] dataBytes = {1, 1, 2, 2, 4, 4, 8, 8, 4, 8, 1};
    public static String[] axisTypes = {"x", "y", "z", "t"};
    private String fileName;
    private Document document;
    
    /**
     * Returns the <code>Document</code> object which represents the xcede file.
     */
    public Document getDocument(){
        return document;
    }
    
    /**
     * Returns the xcede file name.
     * @return  the xcede file name.
     */
    public String getFileName(){
        return fileName;
    }
    /**
     * @see DocumentFactory#createDocument(String).
     */
    public Document createDocument(String fileName) {
        return this.createDocument(new File(fileName));
    }

    /**
     * @see DocumentFactory#createDocument(File).
     */
    public Document createDocument(File file){
        if(file == null || !file.exists()){
            return null;
        }
        fileName = file.getAbsolutePath();
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        try{
            DocumentBuilder builder = factory.newDocumentBuilder();
            document = builder.parse(file);
            return document;
        }catch(SAXParseException e){
            
        }catch(SAXException e){
            
        }catch(ParserConfigurationException e){
            
        }catch(IOException e){
            
        }
        return null;
    }

    /**
     * @see XMLFactory#createDocument(ModelImage).
     */
    public Document createDocument(ModelImage modelImage) {
        if(modelImage == null){
            return null;
        }
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        try {
            FileInfoBase[] fileInfoList = modelImage.getFileInfo();

            DocumentBuilder builder = factory.newDocumentBuilder();
            document = builder.newDocument();
            Element serieslevel = document.createElement("serieslevel");
            document.appendChild(serieslevel);

            Element datarec = document.createElement("datarec");
            serieslevel.appendChild(datarec);
            datarec.setAttribute("type", "image");
            
            Element rasorigin = document.createElement("rasorigin");
            datarec.appendChild(rasorigin);
            float[] raso = getRASOrigin(fileInfoList[0].getOrigin(), fileInfoList[0].getAxisOrientation());
            rasorigin.appendChild(document.createTextNode(floatArrayToString(raso, (String)null)));
            
            int[] extents = fileInfoList[0].getExtents();
            int imageOrientation = fileInfoList[0].getImageOrientation();
            for (int i = 0; i < modelImage.getNDims(); i++) {
                Element dimension = document.createElement("dimension");
                dimension.setAttribute("type", axisTypes[i]);
                datarec.appendChild(dimension);

                Element units = document.createElement("units");
                dimension.appendChild(units);
                units.appendChild(document.createTextNode(FileInfoBase
                        .getUnitsOfMeasureAbbrevStr(fileInfoList[0].getUnitsOfMeasure(i))));
                
                Element size = document.createElement("size");
                dimension.appendChild(size);
                size.appendChild(document.createTextNode(Integer.toString(modelImage.getExtents()[i])));
                
                Element spacing = document.createElement("spacing");
                dimension.appendChild(spacing);
                spacing.appendChild(document.createTextNode(Float.toString(fileInfoList[0].getResolutions()[i])));
                
                Element direction = document.createElement("direction");
                dimension.appendChild(direction);
                int[] axisOrientation = getAxisOrientation(fileInfoList[0].getAxisOrientation(i));
                if(axisOrientation == null){
                    direction.appendChild(document.createTextNode(""));
                }else{
                    direction.appendChild(document.createTextNode(intArrayToString(axisOrientation, " ")));
                }
               
                Element origin = document.createElement("origin");
                dimension.appendChild(origin);
                origin.appendChild(document.createTextNode(Float.toString(fileInfoList[0].getOrigin()[i])));

            }            
            Element elementType = document.createElement("elementtype");
            datarec.appendChild(elementType);
            elementType.appendChild(document.createTextNode(getDataType(fileInfoList[0].getDataType())));
            
            Element byteorder = document.createElement("byteorder");
            datarec.appendChild(byteorder);
            if(fileInfoList[0].getEndianess()){
                byteorder.appendChild(document.createTextNode("msbfirst"));
            }else{
                byteorder.appendChild(document.createTextNode("lsbfirst"));
            }

            int fileFormat = fileInfoList[0].getFileFormat();
            if(isDicom(fileFormat)){
                for(int i = 0; i < fileInfoList.length; i++){
                    Element filename = document.createElement("filename");
                    datarec.appendChild(filename);
                    filename.appendChild(document.createTextNode(fileInfoList[i].getFileName()));

                    Element fileoffset = document.createElement("fileoffset");
                    datarec.appendChild(fileoffset);
                    fileoffset.appendChild(document.createTextNode(Integer.toString(fileInfoList[i].getOffset())));

                    Element filerecordsize = document.createElement("filerecordsize");
                    datarec.appendChild(filerecordsize);
                    filerecordsize.appendChild(document.createTextNode(Integer.toString(fileInfoList[0].getSize())));
                }
            }else{
                Element filename = document.createElement("filename");
                datarec.appendChild(filename);
                filename.appendChild(document.createTextNode(fileInfoList[0].getFileName()));

                Element fileoffset = document.createElement("fileoffset");
                datarec.appendChild(fileoffset);
                fileoffset.appendChild(document.createTextNode(Integer.toString(fileInfoList[0].getOffset())));

                Element filerecordsize = document.createElement("filerecordsize");
                datarec.appendChild(filerecordsize);
                filerecordsize.appendChild(document.createTextNode(Integer.toString(extents[0]*extents[1]*extents[2]*getByteNumber(fileInfoList[0].getDataType()))));
            }
            return document;
        }catch(ParserConfigurationException e){
            return null;
        }
        
    }

    /**
     * @see XMLFactory#saveXML(Document, File).
     */
    public void saveXML(Document document, File file) {
        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            Transformer transformer = factory.newTransformer();

            DOMSource source = new DOMSource(document);
            StreamResult result = new StreamResult(file);
            transformer.transform(source, result);
        } catch (TransformerConfigurationException e) {

        } catch (TransformerException e) {

        }
    }

    /**
     * @see XMLFactory#saveXML(Document, String).
     */
    public void saveXML(Document document, String fileName) {
        this.saveXML(document, new File(fileName));
    }

    public void save(){
        ViewJFrameImage activeImageFrame = ViewUserInterface.getReference().getActiveImageFrame();
        if(activeImageFrame == null){
            return;
        }
        /**
         * Creates the JFileChooser object.
         */
        JFileChooser fileChooser = new JFileChooser();

        /**
         * Sets up the current directory of the JFileChooser.
         */
        String defaultDirectory = Preferences.getProperty(Preferences.PREF_IMAGE_DIR);
        if (defaultDirectory != null) {
            File file = new File(defaultDirectory);

            if (file != null) {
                fileChooser.setCurrentDirectory(file);
            } else {
                fileChooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            fileChooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        /**
         * Sets up the file filter of the JFileChooser.
         */
        fileChooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.XCEDE));

        /**
         * Sets the title of the JFileChooser.
         */
        fileChooser.setDialogTitle("Save XCEDE Schema");

        /**
         * Displays the file chooser dialog and retrieves the user selection.
         */
        int returnValue = fileChooser.showSaveDialog(ViewUserInterface.getReference().getMainFrame());
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            File selectedFile = fileChooser.getSelectedFile();
            fileName = selectedFile.getName();
            String currentDirectory = String.valueOf(fileChooser.getCurrentDirectory()) + File.separatorChar;
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, currentDirectory);
            createDocument(activeImageFrame.getActiveImage());
            saveXML(document, selectedFile);
        }
    }
    
    public Document open(){
        /**
         * Creates the JFileChooser object.
         */
        JFileChooser fileChooser = new JFileChooser();

        /**
         * Sets up the current directory of the JFileChooser.
         */
        String defaultDirectory = Preferences.getProperty(Preferences.PREF_IMAGE_DIR);
        if (defaultDirectory != null) {
            File file = new File(defaultDirectory);

            if (file != null) {
                fileChooser.setCurrentDirectory(file);
            } else {
                fileChooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            fileChooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        /**
         * Sets up the file filter of the JFileChooser.
         */
        fileChooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.XCEDE));

        /**
         * Sets the title of the JFileChooser.
         */
        fileChooser.setDialogTitle("Open XCEDE Schema");

        /**
         * Displays the file chooser dialog and retrieves the user selection.
         */
        int returnValue = fileChooser.showOpenDialog(ViewUserInterface.getReference().getMainFrame());
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            File selectedFile = fileChooser.getSelectedFile();
            fileName = selectedFile.getName();
            String currentDirectory = String.valueOf(fileChooser.getCurrentDirectory()) + File.separatorChar;
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, currentDirectory);
            return open(selectedFile);
        }
        return null;
    }
    
    public Document open(File file){
        return createDocument(file);
    }
    
    public Document open(String fileName){
        return this.open(new File(fileName));
    }
    
    /**
     * Private Helper Functions
     */
    
    /**
     * Returns the triple direction of the given axis orientation according to
     * the image orientation.
     * 
     * @param axisOrientation
     *            the orientation of the given axis.
     * 
     * @return the triple direction of the given axis.
     */
    private static int[] getAxisOrientation(int axisOrientation) {
        int[] direction = new int[3];
        if (axisOrientation == FileInfoBase.ORI_R2L_TYPE) {
            direction[0] = 1;
            direction[1] = 0;
            direction[2] = 0;
        } else if (axisOrientation == FileInfoBase.ORI_L2R_TYPE) {
            direction[0] = -1;
            direction[1] = 0;
            direction[2] = 0;
        } else if (axisOrientation == FileInfoBase.ORI_A2P_TYPE) {
            direction[0] = 0;
            direction[1] = 1;
            direction[2] = 0;
        } else if (axisOrientation == FileInfoBase.ORI_P2A_TYPE) {
            direction[0] = 0;
            direction[1] = -1;
            direction[2] = 0;
        } else if (axisOrientation == FileInfoBase.ORI_I2S_TYPE) {
            direction[0] = 0;
            direction[1] = 0;
            direction[2] = -1;
        } else if (axisOrientation == FileInfoBase.ORI_S2I_TYPE) {
            direction[0] = 0;
            direction[1] = 0;
            direction[2] = 1;
        } else {
            direction = null;
        }
        return direction;
    }
    
    /**
     * Returns the ras origin coordinate.
     * 
     * @param origin           the origin coordinate in the mipav coordinate system.
     * @param axisOrientation  the axis orientation.
     * @return                 the ras origin coordinate.
     */
    public static float[] getRASOrigin(float[] origin, int[] axisOrientation){
        float[] rasorigin = new float[3];
        for (int i = 0; i < 3; i++) {
            if (axisOrientation[i] == FileInfoBase.ORI_R2L_TYPE
                    || axisOrientation[i] == FileInfoBase.ORI_L2R_TYPE) {
                rasorigin[0] = origin[i];
            } else if (axisOrientation[i] == FileInfoBase.ORI_A2P_TYPE
                    || axisOrientation[i] == FileInfoBase.ORI_P2A_TYPE) {
                rasorigin[1] = origin[i];
            } else if (axisOrientation[i] == FileInfoBase.ORI_I2S_TYPE
                    || axisOrientation[i] == FileInfoBase.ORI_S2I_TYPE) {
                rasorigin[2] = origin[i];
            } else {
                return null;
            }
        }
        return rasorigin;
    }
    
    /**
     * Returns the string of the double array.
     * 
     * @param array       the double array
     * @param delimiter   the delimiter between the values of array.
     * @return            the string representation of the double array.
     */
    private static String floatArrayToString(float[] array, String delimiter){
        if(array == null){
            return null;
        }
        
        if(delimiter == null){
            delimiter = " ";
        }
        StringBuffer sb = new StringBuffer("");
        for(int i = 0; i < array.length; i++){
            sb.append(array[i] + delimiter);
        }
        return sb.toString();
    }
    /**
     * Returns the string of the integer array.
     * 
     * @param array
     *            the integer array.
     * @param delimiter
     *            the delimiter string.
     * @return the string representation of the integer array.
     */
    private static String intArrayToString(int[] array, String delimiter){
        if(array == null){
            return null;
        }
        // default delimiter is the space.
        if(delimiter == null){
            delimiter = " ";
        }
        StringBuffer sb = new StringBuffer("");
        for(int i = 0; i < array.length; i++){
            sb.append(array[i] + delimiter);
        }
        return sb.toString();
    }

    /**
     * Returns true if the speicified format is dicom format.
     * @param format  the image file format
     * @return     true if the speicified format is dicom format.
     */
    public static boolean isDicom(int format){
        if(format == FileUtility.DICOM){
            return true;
        }
        return false;
    }
    
    /**
     * Returns the number of bytes of the given data type.
     * @param dataType  a data type
     * @return          the number of bytes which is used to represent this data type.
     */
    public int getByteNumber(int dataType){
        switch(dataType){
        case 1:
            return dataBytes[0];
        case 2:
            return dataBytes[1];
        case 3:
            return dataBytes[2];
        case 4:
            return dataBytes[3];
        case 5:
            return dataBytes[4];
        case 14:
            return dataBytes[5];
        case 6:
            return dataBytes[6];
        case 7:
            return dataBytes[8];
        case 8:
            return dataBytes[9];
        }
        return dataBytes[0];
    }
    
    /**
     * Returns the string representation of the specified data type.
     * @param dataType a data type.
     * @return the string representation of the specified data type.
     */
    public String getDataType(int dataType){
        switch(dataType){
        case 1:
            return dataTypes[0];
        case 2:
            return dataTypes[1];
        case 3:
            return dataTypes[2];
        case 4:
            return dataTypes[3];
        case 5:
            return dataTypes[4];
        case 14:
            return dataTypes[5];
        case 6:
            return dataTypes[6];
        case 7:
            return dataTypes[8];
        case 8:
            return dataTypes[9];
        }
        return dataTypes[10];
    }
    
}
