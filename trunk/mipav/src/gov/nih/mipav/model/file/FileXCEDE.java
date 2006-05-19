package gov.nih.mipav.model.file;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Attr;
import org.xml.sax.*;

import java.io.File;
import java.io.IOException;

import java.util.Vector;

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
import gov.nih.mipav.model.srb.SRBFileTransferer;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.*;

public class FileXCEDE implements DocumentFactory, XMLFactory {
    public static String[] dataTypes = { "int8", "uint8", "int16", "uint16",
            "int32", "uint32", "int64", "uint64", "float32", "float64", "ascii" };

    public static String[] axisTypes = {"x", "y", "z", "t"};
    private String fileName;
    private Document document;
    
    public Document getDocument(){
        return document;
    }
    
    public String getFileName(){
        return fileName;
    }
    
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

    public static boolean isDicom(int format){
        if(format == FileBase.DICOM){
            return true;
        }
        return false;
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

            for (int i = 0; i < modelImage.getNDims(); i++) {
                Element dimension = document.createElement("dimension");
                dimension.setAttribute("type", axisTypes[i]);
                datarec.appendChild(dimension);

                Element units = document.createElement("units");
                dimension.appendChild(units);
                units.appendChild(document.createTextNode(FileInfoBase
                        .getUnitsOfMeasureStr(fileInfoList[0].getUnitsOfMeasure(i))));

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
                    fileoffset.appendChild(document.createTextNode(Integer.toString(((FileInfoDicom)fileInfoList[i]).getOffset())));

                    Element filerecordsize = document.createElement("filerecordsize");
                    datarec.appendChild(filerecordsize);
                    filerecordsize.appendChild(document.createTextNode(Integer.toString(modelImage.getSize())));
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
                filerecordsize.appendChild(document.createTextNode(Integer.toString(modelImage.getSize())));
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
        String defaultDirectory = Preferences.getProperty("ImageDirectory");
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
            Preferences.setProperty("ImageDirectory", currentDirectory);
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
        String defaultDirectory = Preferences.getProperty("ImageDirectory");
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
            Preferences.setProperty("ImageDirectory", currentDirectory);
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
}
