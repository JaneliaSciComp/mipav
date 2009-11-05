import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.LightboxGenerator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileXML.XMLAttributes;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.srb.JDialogLoginSRB;


import java.awt.*;
import java.awt.event.*;
import java.awt.image.MemoryImageSource;
import java.io.*;
import java.net.URL;
import java.util.*;
import java.util.List;
import java.util.zip.*;

import javax.swing.*;
import javax.swing.border.LineBorder;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.axiom.om.OMAttribute;
import org.apache.axiom.om.OMElement;
import org.apache.axiom.om.impl.builder.StAXOMBuilder;
import org.jdom.Document;
import org.jdom.input.SAXBuilder;
import org.jdom.Attribute;
import org.jdom.Element;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;

import com.sun.jimi.core.Jimi;
import com.sun.jimi.core.JimiException;




public class PlugInDialogNDAR extends JDialogStandalonePlugin implements ActionListener, ChangeListener, ItemListener, TreeSelectionListener {

    /** Scrolling text area for log output */
    private WidgetFactory.ScrollTextArea logOutputArea;

    private JScrollPane listPane;

    private JList sourceList;

    private JButton addSourceButton, finishButton, removeSourceButton;

    private DefaultListModel sourceModel;

    private static final String outputDirBase = System.getProperty("user.home") + File.separator + "mipav"
            + File.separator + "NDAR_Imaging_Submission" + File.separator;

    /** Length of the NDAR GUID */
    private static final int GUID_LENGTH = 12;

    private Hashtable<File, Boolean> multiFileTable = null;
    
    private Hashtable<File, LinkedHashMap<String,String>> infoTable = null;
    
    private Hashtable<File, String> outputFileNameBaseTable = null;
    
    private ArrayList<DataStruct> xmlDataStructs;
    
    /** tab level counter for writing xml header. */
    protected int tabLevel = 0;

    /** Buffered writer for writing to XML file*/
    protected BufferedWriter bw;
    
    protected FileWriter fw;
    
    protected static final String TAB = "\t";
    
    /** XML encoding string. */
    protected static final String XML_ENCODING = "UTF-8";

    
    
    public PlugInDialogNDAR() {
        super(false);
        Icon icon = null;
        try {
        	icon = new ImageIcon(MipavUtil.getIconImage(Preferences.getIconName()));
        }catch(Exception e) {
        	
        }
        int response = JOptionPane.showConfirmDialog(this, JDialogLoginSRB.NDAR_PRIVACY_NOTICE,
                "NDAR Image Submission Package Creation Tool", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        if (response == JOptionPane.YES_OPTION) {
        	init();
            setVisible(true);
            validate();
        }else {
        	return;
        }
        
    }

    public void actionPerformed(ActionEvent e) {

        /*
         * @todo Implement this java.awt.event.ActionListener abstract method
         */

        String command = e.getActionCommand();

        // System.err.println("size : " + this.getSize());

       if (command.equals("AddSource")) {
            ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
            fileChooser.setMulti(ViewUserInterface.getReference().getLastStackFlag());

            JFileChooser chooser = fileChooser.getFileChooser();
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));

            // default to TECH filter
            int filter = ViewImageFileFilter.TECH;

            try {
                filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
            } catch (NumberFormatException nfe) {

                // an invalid value was set in preferences -- so don't use it!
                filter = -1;
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

            if (filter != -1) {
                // it seems that the set command adds the filter again...
                // chooser.addChoosableFileFilter(new ViewImageFileFilter(filter));

                // if filter is something we already added, then remove it before
                // setting it..... (kludgy, kludgy....)
                javax.swing.filechooser.FileFilter found = ViewOpenFileUI.findFilter(chooser, filter);

                if (found != null) {
                    chooser.removeChoosableFileFilter(found);
                }

                // initially set to the preferences
                chooser.setFileFilter(new ViewImageFileFilter(filter));
            }

            int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                boolean isMultiFile = fileChooser.isMulti();

                File[] files = chooser.getSelectedFiles();
                ViewUserInterface.getReference().setDefaultDirectory(files[0].getParent());
                for (int i = 0; i < files.length; i++) {
                    if ( !sourceModel.contains(files[i])) {
                        sourceModel.addElement(files[i]);
                        multiFileTable.put(files[i], new Boolean(isMultiFile));
                        new InfoDialog(this,files[i]);
                    }
                }
            }
            removeSourceButton.setEnabled(sourceModel.size() > 0);
            finishButton.setEnabled(sourceModel.size() > 0);
            listPane.setBorder(buildTitledBorder(sourceModel.size() + " image(s) "));

        } else if (command.equals("RemoveSource")) {
            int[] selected = sourceList.getSelectedIndices();
            for (int i = selected.length - 1; i >= 0; i--) {
            	File f = (File)sourceModel.elementAt(selected[i]);
                sourceModel.removeElementAt(selected[i]);
                //multiFileTable.remove(selected[i]);
                multiFileTable.remove(f);
                infoTable.remove(f);
                outputFileNameBaseTable.remove(f);
            }
            removeSourceButton.setEnabled(sourceModel.size() > 0);
            finishButton.setEnabled(sourceModel.size() > 0);
            listPane.setBorder(buildTitledBorder(sourceModel.size() + " image(s) "));
        }else if (command.equals("Help")) {
          
        	//MipavUtil.showHelp("ISPImages01");

        }else if(command.equals("Finish")) {
        	final gov.nih.mipav.SwingWorker worker = new gov.nih.mipav.SwingWorker() {
                public Object construct() {
                    createSubmissionFiles();

                    return null;
                }
            };
            int response = JOptionPane.showConfirmDialog(this, "Done adding image datasets?",
                    "Done adding image datasets?", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

            if (response == JOptionPane.YES_OPTION) {
            	worker.start();
            	removeSourceButton.setEnabled(false);
            	finishButton.setEnabled(false);
            	addSourceButton.setEnabled(false);
            	
            }
            
        }
    }

    public void stateChanged(ChangeEvent e) {
       
    }

    public void itemStateChanged(ItemEvent e) {
   
    }


    private void init() {
        setTitle("NDAR Image Submission Package Creation Tool");

        multiFileTable = new Hashtable<File, Boolean>();
        infoTable = new Hashtable<File, LinkedHashMap<String,String>>();
        outputFileNameBaseTable = new Hashtable<File, String>();

        getContentPane().add(buildSourcePanel(), BorderLayout.NORTH);
        
        getContentPane().add(buildLogPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtonPanel(), BorderLayout.SOUTH);
        pack();
        validate();
        this.setMinimumSize(new Dimension(610, 437));
        this.setSize(new Dimension(610, 437));
    }

    
    /**
     * Build a panel for the zip and metadata file creation log.
     */
    private JPanel buildLogPanel() {
        JPanel destPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.gridy = 0;
        gbc2.gridx = 0;

        logOutputArea = WidgetFactory.buildScrollTextArea(Color.white);
        logOutputArea.setBorder(buildTitledBorder("Output log"));
        logOutputArea.getTextArea().setEditable(false);

        destPanel.add(logOutputArea, gbc2);

        return destPanel;
    }
    

    public void valueChanged(TreeSelectionEvent e) {

	}

	private JScrollPane buildSourcePanel() {
        //JPanel sourcePanel = new JPanel();
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;

        sourceModel = new DefaultListModel();
        sourceList = new JList(sourceModel);

        listPane = WidgetFactory.buildScrollPane(sourceList);
        listPane.setBorder(buildTitledBorder(0 + " image(s) "));
        //sourcePanel.add(listPane, gbc);

        return listPane;
    }



    /**
     * Checks to see if the given string is a valid NDAR GUID
     * 
     * @param checkString the string to check
     * @return whether this is a valid guid
     */
    private boolean isValidGUID(String checkString) {
        if (checkString.length() != GUID_LENGTH) {
            return false;
        }

        if (isValidChar(checkString.charAt(4)) && isValidChar(checkString.charAt(5))
                && isNumChar(checkString.charAt(6)) && isNumChar(checkString.charAt(7))
                && isNumChar(checkString.charAt(8)) && isValidChar(checkString.charAt(9))
                && isValidChar(checkString.charAt(10))
                && (isNumChar(checkString.charAt(11)) || isValidChar(checkString.charAt(11)))) {
            return true;
        }
        return false;
    }

    /**
     * Is the char a valid number character
     * 
     * @param checkChar char to check
     * @return whether is a number
     */
    private boolean isNumChar(char checkChar) {
        return (checkChar >= '0' && checkChar <= '9');
    }

    /**
     * Check if this is a valid NDAR character ( no I, O, Q, or S)
     * 
     * @param checkChar char to check
     * @return is the char valid
     */
    private boolean isValidChar(char checkChar) {
        if ( (checkChar >= 'a' && checkChar <= 'z') || (checkChar >= 'A' && checkChar <= 'Z')) {
            if (checkChar != 'i' && checkChar != 'I' && checkChar != 'o' && checkChar != 'O' && checkChar != 'q'
                    && checkChar != 'Q' && checkChar != 's' && checkChar != 'S') {
                return true;
            }
        }

        return false;
    }

    /**
     * Create the ZIP(s) containing the original image files and the XML meta-data for each image dataset.
     */
    private void createSubmissionFiles() {
        if ( !new File(outputDirBase).exists()) {
            new File(outputDirBase).mkdirs();
        }

        int numImages = sourceModel.size();
        for (int i = 0; i < numImages; i++) {
            File imageFile = (File) sourceModel.elementAt(i);
            String guid = outputFileNameBaseTable.get(imageFile);
            printlnToLog("Opening: " + imageFile + ", multifile: " + multiFileTable.get(imageFile));

            // ViewJFrameImage invisFrame = new ViewJFrameImage(tempImage);

            FileIO fileIO = new FileIO();
            fileIO.setQuiet(true);
            ModelImage origImage = fileIO.readImage(imageFile.getName(), imageFile.getParent() + File.separator,
                    multiFileTable.get(imageFile), null);

            List<String> origFiles = FileUtility.getFileNameList(origImage);
            
            
            //create a thumbnail image...4 colums, 2 rows
            //grab the middle 8 slices from the image for the thumbnail
            //need to determine by what percentage...so...need to figure out by what percebtahe the xdim will go down to 128
            //startSLice will be 3 less than middle slice 
            //endSlice will be 4 more than middle slixe
            int xDim = origImage.getExtents()[0];
            int percentage = 100;
            if(xDim > 128) {
            	float perc = 128f/xDim * 100;
            	percentage = (int)Math.floor(perc);
            }
            int columns = 4;
            int rows = 2;
            int rBorderVal = 255;
            int gBorderVal = 0;
            int bBorderVal = 0;
            int borderThick = 1;
            int startSlice = 0;
            int endSlice = 0;
            int numSlices = 0;
            int middleSlice = 0;
            LightboxGenerator lightGen;
            ModelImage thumbnailImage = null;
            if(origImage.is2DImage()) {
            	//Creating a blank TransMatrix for resampling
        		TransMatrix percentSizer = new TransMatrix(4);
        		percentSizer.Set((float)1, (float)0, (float)0, (float)0, (float)0,
        				(float)1, (float)0, (float)0, (float)0, (float)0, (float)1, (float)0, 
        				(float)0, (float)0, (float)0, (float)1);
            	
        		//Resample image size based on percent inputted
            	AlgorithmTransform transformer = new AlgorithmTransform(origImage, percentSizer, 1, (float)(origImage.getResolutions(0)[0]/(percentage*.01)),
            			(float)(origImage.getResolutions(0)[1]/(percentage*.01)), (int)(origImage.getExtents()[0] * percentage*.01),
            			(int)(origImage.getExtents()[1]*percentage*.01), origImage.getUnitsOfMeasure(), false, true, false, true, origImage.getImageCentermm(false) );
            	transformer.runAlgorithm();
            	thumbnailImage = transformer.getTransformedImage();
            	thumbnailImage.calcMinMax();
            	//convert this image to color image if it is not
        		if(!thumbnailImage.isColorImage()) {
        			ModelImage newRGB = new ModelImage(ModelImage.ARGB, thumbnailImage.getExtents(), thumbnailImage.getImageName());
    		    	AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(thumbnailImage, thumbnailImage, thumbnailImage, newRGB, true, true, 255.0f, true);
    		    	mathAlgo.run();
    		    	thumbnailImage.disposeLocal();
    		    	thumbnailImage = null;
    		    	thumbnailImage = newRGB;
        		}
            }else if(origImage.is3DImage()) {
            	numSlices = origImage.getExtents()[2];
            	numSlices = numSlices - 1;  //its 0 based
            	middleSlice = numSlices/2;
            	startSlice = middleSlice - 3;
            	if(startSlice < 0) {
            		startSlice = 0;
            	}
            	endSlice = middleSlice + 4;
            	if(endSlice > numSlices - 1) {
            		endSlice = numSlices - 1;
            	}
            	
            	try {
                    // Make algorithm
            		lightGen = new LightboxGenerator(origImage, startSlice, endSlice, percentage, rows, columns, rBorderVal, gBorderVal, bBorderVal, false, borderThick);
            		lightGen.run();
            		thumbnailImage = lightGen.getImage();
            		thumbnailImage.calcMinMax();
            	}catch(Exception e) {
            		e.printStackTrace();
            	}	
            }else if(origImage.is4DImage()) {
            	//get middle time volume
            	int[] destExtents = new int[3]; 
                int xSlices = origImage.getExtents()[0];
                int ySlices = origImage.getExtents()[1];
                int zSlices = origImage.getExtents()[2];
                destExtents[0] = xSlices;
                destExtents[1] = ySlices;
                destExtents[2] = zSlices;
                
                ModelImage timeImage = new ModelImage(origImage.getType(), destExtents, "");

                int tSlices =  origImage.getExtents()[3];
                int middleVol = (int)Math.floor(tSlices/2);
                if(middleVol > 0) {
                	middleVol = middleVol - 1;  // 0 based
                }
                AlgorithmSubset subsetAlgo = new AlgorithmSubset(origImage, timeImage, AlgorithmSubset.REMOVE_T, middleVol);
                subsetAlgo.run();
                
                numSlices = timeImage.getExtents()[2];
            	numSlices = numSlices - 1;  //its 0 based
            	middleSlice = numSlices/2;
            	startSlice = middleSlice - 3;
            	if(startSlice < 0) {
            		startSlice = 0;
            	}
            	endSlice = middleSlice + 4;
            	if(endSlice > numSlices - 1) {
            		endSlice = numSlices - 1;
            	}
            	try {
                    // Make algorithm
            		lightGen = new LightboxGenerator(timeImage, startSlice, endSlice, percentage, rows, columns, rBorderVal, gBorderVal, bBorderVal, false, borderThick);
            		lightGen.run();
            		thumbnailImage = lightGen.getImage();
            		thumbnailImage.calcMinMax();
            		if(timeImage != null) {
            			timeImage.disposeLocal();
            			timeImage = null;
            		}	
            	}catch(Exception e) {
            		
            	}	
            }

            int modality = origImage.getFileInfo(0).getModality();
            String modalityString = FileInfoBase.getModalityStr(modality).replaceAll("\\s+", "");

            String outputFileNameBase;
            if (modality == FileInfoBase.UNKNOWN_MODALITY) {
                outputFileNameBase = guid + "_" + System.currentTimeMillis();
            } else {
                outputFileNameBase = guid + "_" + modalityString + "_" + System.currentTimeMillis();
            }

            String zipFilePath = outputDirBase + outputFileNameBase + ".zip";
            
            //write out thumbnail image
            FileWriteOptions opts = new FileWriteOptions(outputFileNameBase + ".jpg", outputDirBase, true);
            writeThumbnailJIMI(thumbnailImage, opts);
            if(thumbnailImage != null) {
            	thumbnailImage.disposeLocal();
            	thumbnailImage = null;
            }
            printlnToLog("Creating thumbnail image:\t" + outputDirBase + outputFileNameBase + ".jpg");
            try {
                printlnToLog("Creating ZIP file:\t" + zipFilePath);
                for (String file : origFiles) {
                    printlnToLog("Adding file to ZIP:\t" + file);
                }

                makeZipFile(zipFilePath, origFiles);
            } catch (IOException ioe) {
                ioe.printStackTrace();
                MipavUtil.displayError("Unable to write original image dataset files to ZIP package:\n"
                        + ioe.getMessage());
                continue;
            }


            //now we need to write out the xml...nish
            writeXMLFile(outputDirBase, outputFileNameBase, imageFile, origImage);

            origImage.disposeLocal();

            printlnToLog("");
        }

        printlnToLog("*** Submission package processing complete. ***");

    }

    //nish
    private void writeXMLFile(String outputDirBase, String outputFileNameBase, File imageFile, ModelImage origImage) {
    	String xmlFileName = outputFileNameBase + ".xml";
    	String xmlHeader = "<?xml version=\"1.0\" ?>";
    	String xmlSchema = "http://www.w3.org/2001/XMLSchema-instance";
    	String xsd = "schema.xsd";

    	
    	try {
	    	File xmlFile = new File(outputDirBase + xmlFileName);
	        fw = new FileWriter(xmlFile);
	        bw = new BufferedWriter(fw);
	        bw.write(xmlHeader);
	        bw.newLine();
	        openTag("data_set xmlns:xsi=\"" + xmlSchema + "\" xsi:noNamespaceSchemaLocation=\"" + xsd + "\"", true);
	        for(int i=0;i<xmlDataStructs.size();i++) {
	        	DataStruct ds = xmlDataStructs.get(i);
	        	String n = ds.getName();
	        	String v = ds.getVersion();
	        	openTag("data_structure name=\"" + n + "\" version=\"" + v + "\"", true);
	        	parse(ds,imageFile, outputFileNameBase);
	        	openTag("data_structure", false);
	        	
	        }
	        
	        
	        
	        
	        
	        
	        
	        
	        openTag("data_set", false);
	        bw.close();

    	}catch(Exception e) {
    		e.printStackTrace();
    		
    	}
    	
    	
        
        
    }
    
    
    
    /**
	 * 
	 * @param ds
	 */
	private void parse(DataStruct ds2,File imageFile, String outputFileNameBase) {
		Vector<XMLAttributes> attr;
		XMLAttributes xmlAttributes;
		LinkedHashMap<String,String> infoMap;
		
		for(int k=0;k<ds2.size();k++) {
			
			Object o1 = ds2.get(k);
			if(o1 instanceof DataElement) {
				//data element
				DataElement de = (DataElement)o1;
				String name = de.getName();
				String value = "";
				String v;
				if(name.equals("image_file")) {
        			value = outputFileNameBase + ".zip";
        		}else if (name.equals("image_thumbnail_file")) {
        			value = outputFileNameBase + ".jpg";
        		}else {
					//need to get appropriat value
					infoMap = infoTable.get(imageFile);
					Set keySet = infoMap.keySet();
					Iterator iter = keySet.iterator();
					String key;
					 while (iter.hasNext()) {
				        	key = (String)iter.next();
				        	if(key.equals(name)) {
				        		v = infoMap.get(key);
				        		value = v;
				        		break;
				        		
				        	}
					}
        		}
				if(!value.trim().equals("")) {
					attr = new Vector<XMLAttributes>();
					xmlAttributes = new XMLAttributes("name",name);
					attr.add(xmlAttributes);
					xmlAttributes = new XMLAttributes("value",value);
					attr.add(xmlAttributes);
					closedTag("data_element", attr);
				}
			}else {
				DataStruct ds3 = (DataStruct)o1;
				String n = ds3.getName();
				String v = ds3.getVersion();
				openTag("data_structure name=\"" + n + "\" version=\"" + v + "\"", true);
				parse(ds3, imageFile, outputFileNameBase);
				openTag("data_structure", false);
			}
			
			
		}
	}
    
    
    /**
     * Simple function to write an xml formatted open ended tag (value not included).
     *
     * @param  bw     writer to use
     * @param  tag    tag name
     * @param  start  is this a start or end tag
     */
    public final void openTag(String tag, boolean start) {

        try {

            if (!start) {

                // done with this container
                tabLevel--;
            }

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            if (start) {
                bw.write("<" + tag + ">");

                // indent the contained tags
                tabLevel++;
            } else {
                bw.write("</" + tag + ">");
            }

            bw.newLine();
        } catch (IOException ex) { }
    }
    
    
    /**
     * Simple function to write an xml formatted closed tag including the tag value.
     *
     * @param  bw   write to use
     * @param  tag  tag name
     * @param  val  tag value
     */
    protected final void closedTag(String tag, String val) {

        try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            // entity-ize some xml-unfriendly characters and convert to the XML charset
            String writeVal = val.trim().replaceAll("&", "&amp;");
            writeVal = writeVal.trim().replaceAll("\"", "&quot;");
            writeVal = writeVal.trim().replaceAll("<", "&lt;");
            writeVal = writeVal.trim().replaceAll(">", "&gt;");
            writeVal = new String(writeVal.getBytes(XML_ENCODING));

            bw.write("<" + tag + ">" + writeVal + "</" + tag + ">");
            bw.newLine();
        } catch (IOException ex) { }
    }
    
    
    /**
	 * Writes a closed tag where no value is specified, only attributes.
	 */
	public final void closedTag(String tag, Vector<XMLAttributes> attr) {
    	
		try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(TAB);
            }

            bw.write("<" + tag);
            
            String attrStr;
            for (int i = 0; i < attr.size(); i++) {
            	
            	attrStr = attr.elementAt(i).getValue().trim().replaceAll("&", "&amp;");
            	attrStr = attrStr.trim().replaceAll("\"", "&quot;");
            	attrStr = attrStr.trim().replaceAll("<", "&lt;");
            	attrStr = attrStr.trim().replaceAll(">", "&gt;");
            	attrStr = new String(attrStr.getBytes(XML_ENCODING));
            	
            	bw.write(" " + attr.elementAt(i).getName() + "=\"" + attrStr + "\"");
            }
            
            bw.write("/>");

            bw.newLine();
        } catch (IOException ex) { }
		
		attr.clear();
    }
	

    /**
     * Adds a set of files to a ZIP archive.
     * 
     * @param destZipFile The full path to the ZIP archive to create.
     * @param srcFiles A list of files (full paths) to include in the ZIP archive.
     * @throws IOException If there is a problem reading the srcFiles or writing to the ZIP file.
     */
    private void makeZipFile(String destZipFile, List<String> srcFiles) throws IOException {
        // Create a buffer for reading the files
        byte[] buf = new byte[1024];

        // Create the ZIP file
        ZipOutputStream out = new ZipOutputStream(new FileOutputStream(destZipFile));

        // Compress the files
        for (String file : srcFiles) {
            FileInputStream in = new FileInputStream(file);

            // Add ZIP entry to output stream.
            out.putNextEntry(new ZipEntry(FileUtility.getFileName(file)));

            // Transfer bytes from the file to the ZIP file
            int len;
            while ( (len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }

            // Complete the entry
            out.closeEntry();
            in.close();
        }

        // Complete the ZIP file
        out.close();
    }

    /**
     * Append a line to the log output area in the Log tab.
     * 
     * @param line The line to append (do not include the trailing newline).
     */
    private void printlnToLog(String line) {
        logOutputArea.getTextArea().append(line + "\n");
    }

    private JPanel buildButtonPanel() {
    	
        JPanel buttonPanel1 = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;

        addSourceButton = WidgetFactory.buildTextButton("Add images", "Add image datasets", "AddSource", this);
        removeSourceButton = WidgetFactory.buildTextButton("Remove images", "Remove the selected image datasets", "RemoveSource", this);
        finishButton = WidgetFactory.buildTextButton("Finish", "Finish", "Finish", this);
        //helpButton = WidgetFactory.buildTextButton("Help", "Show MIPAV help", "Help", this);

        addSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
        finishButton.setPreferredSize(MipavUtil.defaultButtonSize);
        //helpButton.setPreferredSize(MipavUtil.defaultButtonSize);


        addSourceButton.setEnabled(true);
        removeSourceButton.setEnabled(false);
        finishButton.setEnabled(false);

        gbc.gridx = 0;
        buttonPanel1.add(addSourceButton,gbc);
        gbc.gridx = 1;
        buttonPanel1.add(removeSourceButton,gbc);
        gbc.gridx = 2;
        buttonPanel1.add(finishButton,gbc);


        return buttonPanel1;
    }
    
    
    
    /**
     * Writes a JIMI file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeThumbnailJIMI(ModelImage image, FileWriteOptions options) {
    	int imageSize = image.getExtents()[0] * image.getExtents()[1];
        int[] paintBuffer = new int[imageSize];
        ColorRGBA colorMappedA = new ColorRGBA();
        float[] imageBufferA = new float[image.getExtents()[0] * image.getExtents()[1] * 4];
        int length = imageBufferA.length;
        ColorRGB[] m_akOffset = { new ColorRGB(0.0f, 0.0f, 0.0f), new ColorRGB(0.0f, 0.0f, 0.0f) };
        float fMaxColor = 255;
        float[] m_afNormColor = { 1, 1 };
        
        
        if (image.getMinR() < 0.0) {
            fMaxColor = (float) (image.getMaxR() - image.getMinR());
            m_akOffset[0].R = (float) (-image.getMinR());
        } else {
            fMaxColor = (float) image.getMaxR();
        }

        if (image.getMinG() < 0.0) {
            fMaxColor = Math.max((float) (image.getMaxG() - image.getMinG()), fMaxColor);
            m_akOffset[0].G = (float) (-image.getMinG());
        } else {
            fMaxColor = Math.max((float) image.getMaxG(), fMaxColor);
        }

        if (image.getMinB() < 0.0) {
            fMaxColor = Math.max((float) (image.getMaxB() - image.getMinB()), fMaxColor);
            m_akOffset[0].B = (float) (-image.getMinB());
        } else {
            fMaxColor = Math.max((float) image.getMaxB(), fMaxColor);
        }
        m_afNormColor[0] = 255 / fMaxColor;
        
        try {
        	image.exportData(0, length, imageBufferA);
        }catch(Exception e) {
        	
        }
        for (int j = 0; j < image.getExtents()[1]; j++) {

            for (int i = 0; i < image.getExtents()[0]; i++) {
                int ind4 = (j * image.getExtents()[0]) + i;
                int index = 4 * ind4;
                int pixValue;
                
                colorMappedA.R = 0;
                colorMappedA.G = 0;
                colorMappedA.B = 0;
                colorMappedA.A = imageBufferA[index];
                
                colorMappedA.R = (imageBufferA[index + 1] + m_akOffset[0].R) * m_afNormColor[0];
                colorMappedA.G = (imageBufferA[index + 2] + m_akOffset[0].G) * m_afNormColor[0];
                colorMappedA.B = (imageBufferA[index + 3] + m_akOffset[0].B) * m_afNormColor[0];
                
                pixValue = 0xff000000 | ((int) (colorMappedA.R) << 16) | ((int) (colorMappedA.G) << 8) | ((int) (colorMappedA.B));
                
                paintBuffer[ind4] = pixValue;
            } 
        }
        
        MemoryImageSource memImageA = new MemoryImageSource(image.getExtents()[0], image.getExtents()[1], paintBuffer, 0, image.getExtents()[0]);

        int extIndex = options.getFileName().indexOf(".");
        String prefix = options.getFileName().substring(0, extIndex); // Used for setting file name
        String fileSuffix = options.getFileName().substring(extIndex);
        String name;

        Image img = createImage(memImageA);

        name = options.getFileDirectory() + prefix + fileSuffix;


        try {
            Jimi.putImage(img, name);
        } catch (JimiException jimiException) {
            Preferences.debug("JIMI write error: " + jimiException + "\n", Preferences.DEBUG_FILEIO);

            jimiException.printStackTrace();

            return false;
        }



        return true;
    }
    

  
    /**
     * launches the dialog to add info
     * @author pandyan
     *
     */
    private class InfoDialog extends JDialog implements ActionListener, WindowListener {
    	private Dialog owner;
    	private File file;
    	private ArrayList<JComponent> components = new ArrayList<JComponent>();
    	private ArrayList<JLabel> labels = new ArrayList<JLabel>();
    	private String imageXMLFilePath;
    	private JPanel mainPanel;
    	private GridBagConstraints gbc;
    	private JScrollPane scrollPane;
    	private LinkedHashMap<String,String> infoMap;
    	private String guid = "";
    	private DataStruct dataStruct;
    	private FileInputStream inputStream;
    	private ModelImage origImage;
    	private FileIO fileIO;

    	
    	public InfoDialog(Dialog owner, File file) {
    		
    		super(owner,true);
    		URL xmlURL = getClass().getClassLoader().getResource("image_dictionary.xml");
    		 if (xmlURL == null) {
                 MipavUtil.displayError("Unable to find XML : " + "image_dictionary.xml");
                 this.dispose();
                 owner.dispose();
                 return;
             }
    		imageXMLFilePath = xmlURL.getPath();
    		//in future....retrieve OMElement from web service
    		
    		this.owner = owner;
    		this.file = file;
    		this.infoMap = new LinkedHashMap<String,String>();
    		xmlDataStructs = new ArrayList<DataStruct>();
    		fileIO = new FileIO();
            fileIO.setQuiet(true);
            origImage = fileIO.readImage(file.getName(), file.getParent() + File.separator,multiFileTable.get(file), null);
    		init();
    		if(origImage != null) {
    			origImage.disposeLocal();
    			origImage = null;
    		}
    	}
    	
    	/**
    	 * init
    	 */
    	private void init() {
    		setTitle("Add info for " + file.getName());

    		addWindowListener(this);
    		mainPanel = new JPanel(new GridBagLayout());
    		scrollPane = new JScrollPane(mainPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

            gbc = new GridBagConstraints();
    		
    		try {
    			this.setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
    			//image xml

    			inputStream = new FileInputStream(new File(imageXMLFilePath));
    			StAXOMBuilder stAXOMBuilder = new StAXOMBuilder(inputStream);
    			OMElement documentElement = stAXOMBuilder.getDocumentElement();
    			Iterator iter = documentElement.getChildElements();
    			OMElement childElement;
    			OMAttribute attr;
    			QName qname;
    			while(iter.hasNext()) {
    				childElement = (OMElement)iter.next();
    				qname = new QName("name");
    				attr = childElement.getAttribute(qname);
    				String n = attr.getAttributeValue();
    				
    				qname = new QName("short_name");
    				attr = childElement.getAttribute(qname);
    				String s = attr.getAttributeValue();
    				
    				qname = new QName("desc");
    				attr = childElement.getAttribute(qname);
    				String d = attr.getAttributeValue();
    				
    				qname = new QName("version");
    				attr = childElement.getAttribute(qname);
    				String v = attr.getAttributeValue();
    				
    				qname = new QName("type");
    				attr = childElement.getAttribute(qname);
    				String t = attr.getAttributeValue();
    				
    				dataStruct = new DataStruct(n,s,d,v,t);
    				parse(childElement, dataStruct);
    				xmlDataStructs.add(dataStruct);
    			}

    		}catch(Exception e) {
    			e.printStackTrace();
    		}

    		gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.insets = new Insets(15,5,5,15);
            gbc.gridwidth = 1;
            

    		for(int i=0;i<labels.size();i++) {
    			JLabel l = labels.get(i);
    			JComponent t = components.get(i);
    			gbc.anchor = GridBagConstraints.EAST;
    			mainPanel.add(l,gbc);
    			gbc.gridx = 1;
    			gbc.anchor = GridBagConstraints.WEST;
    			mainPanel.add(t,gbc);
    			
    			gbc.gridy = gbc.gridy + 1;
    			gbc.gridx = 0;
    			
    			
    		}
    		 JPanel OKPanel = new JPanel();
    	     buildOKButton();
    	     OKButton.setActionCommand("ok3");
             OKButton.addActionListener(this);
    	     OKPanel.add(OKButton, BorderLayout.CENTER);
    		
    	     
    	     
    	    populateFields();
    		
    		getContentPane().add(scrollPane, BorderLayout.CENTER);
    		getContentPane().add(OKPanel, BorderLayout.SOUTH);
    		pack();
    		this.setMinimumSize(new Dimension(570, 400));
            this.setSize(new Dimension(570, 400));
            MipavUtil.centerInWindow(owner, this);
            setResizable(false);
            setVisible(true);
    	}
    	
    	
		
		/**
    	 * parses the OMElement
    	 * @param ds
    	 */
		private void parse(OMElement omElement, DataStruct ds2) {
			Iterator iter = omElement.getChildElements();
			OMElement childElement;
			OMAttribute attr;
			QName qname;
			String childElementName;
			JTextField tf;
			JComboBox cb;
			while(iter.hasNext()) {
				childElement = (OMElement)iter.next();
				childElementName = childElement.getLocalName();
				if(childElementName.equals("data_element")) {
					qname = new QName("name");
    				attr = childElement.getAttribute(qname);
    				String n = attr.getAttributeValue();

    				qname = new QName("desc");
    				attr = childElement.getAttribute(qname);
    				String d = attr.getAttributeValue();
    				
    				qname = new QName("short_desc");
    				attr = childElement.getAttribute(qname);
    				String sh = attr.getAttributeValue();

    				qname = new QName("type");
    				attr = childElement.getAttribute(qname);
    				String t = attr.getAttributeValue();

    				qname = new QName("size");
    				attr = childElement.getAttribute(qname);
    				String s = attr.getAttributeValue();

    				qname = new QName("required");
    				attr = childElement.getAttribute(qname);
    				String r = attr.getAttributeValue();

    				qname = new QName("value_range");
    				attr = childElement.getAttribute(qname);
    				String v = attr.getAttributeValue();

    				
    				DataElement de = new DataElement(n,d,sh,t,s,r,v);
    				ds2.add(de);
    				
    				if(!(n.equals("image_file") || n.equals("image_thumbnail_file") || (origImage.is2DImage() && n.equals("image_extent3")) || ((origImage.is2DImage() || origImage.is3DImage()) && n.equals("image_extent4")) || ((origImage.is2DImage() || origImage.is3DImage() || origImage.is4DImage()) && n.equals("image_extent5")) || (origImage.is2DImage() && n.equals("image_resolution3")) || ((origImage.is2DImage() || origImage.is3DImage()) && n.equals("image_resolution4")) || ((origImage.is2DImage() || origImage.is3DImage() || origImage.is4DImage()) && n.equals("image_resolution5")) || (origImage.is2DImage() && n.equals("image_unit3")) || ((origImage.is2DImage() || origImage.is3DImage()) && n.equals("image_unit4")) || ((origImage.is2DImage() || origImage.is3DImage() || origImage.is4DImage()) && n.equals("image_unit5")))) {
    			
						JLabel l = new JLabel(n);
						//System.out.println(l.getText());
						//if valuerange is enumeration, create a combo box...otherwise create a textfield
						if(v.contains(";")) {
							cb = new JComboBox();
							String[] items = v.split(";");
							for(int i=0;i<items.length;i++) {
								String item = items[i].trim();
								 cb.addItem(item);
							}
							components.add(cb);
						}else {
							tf = new JTextField(30);
							components.add(tf);
						}
						labels.add(l);
					}
				}else if(childElementName.equals("data_structure")) {
					qname = new QName("name");
    				attr = childElement.getAttribute(qname);
    				String n = attr.getAttributeValue();
    				
    				qname = new QName("short_name");
    				attr = childElement.getAttribute(qname);
    				String s = attr.getAttributeValue();
    				
    				qname = new QName("desc");
    				attr = childElement.getAttribute(qname);
    				String d = attr.getAttributeValue();
    				
    				qname = new QName("version");
    				attr = childElement.getAttribute(qname);
    				String v = attr.getAttributeValue();
    				
    				qname = new QName("type");
    				attr = childElement.getAttribute(qname);
    				String t = attr.getAttributeValue();
    				
    				dataStruct = new DataStruct(n,s,d,v,t);
    				parse(childElement, dataStruct);
    				ds2.add(dataStruct);
				}
			}
    	}
    	
    	/**
    	 * prepopulates some of the fields with info from image header
    	 */
    	public void populateFields() {

            float[] res = origImage.getResolutions(0);
            int[] units = origImage.getUnitsOfMeasure();
            int exts[] = origImage.getExtents();
            int nDims = origImage.getNDims();
            int modality = origImage.getFileInfo(0).getModality();
            String modalityString = FileInfoBase.getModalityStr(modality);
            float sliceThickness = origImage.getFileInfo(0).getSliceThickness();
            int orient = origImage.getFileInfo(0).getImageOrientation();
            String orientation = FileInfoBase.getImageOrientationStr(orient);

            	//get index for extents
            	for(int i=0;i<labels.size();i++) {
            		String l = labels.get(i).getText();
            		
            		if(l.equals("image_num_dimensions")) {
            				((JTextField)components.get(i)).setText(String.valueOf(nDims));
            				components.get(i).setEnabled(false);
            		}else if(l.equals("image_extent1")) {
            				((JTextField)components.get(i)).setText(String.valueOf(exts[0]));
            				 components.get(i).setEnabled(false);
            		}else if(l.equals("image_extent2")) {
            				((JTextField)components.get(i)).setText(String.valueOf(exts[1]));
            				components.get(i).setEnabled(false);
            		}else if(l.equals("image_extent3")) {
                			((JTextField)components.get(i)).setText(String.valueOf(exts[2]));
                			components.get(i).setEnabled(false);
            		}else if(l.equals("image_extent4")) {
                			((JTextField)components.get(i)).setText(String.valueOf(exts[3]));
                			components.get(i).setEnabled(false);
            		}else if(l.equals("image_extent5")) {
            			//for now just disabling it
            			components.get(i).setEnabled(false);
            		}else if(l.equals("image_unit1")) {

            				JComboBox jc = (JComboBox)components.get(i);
            				for(int k=0;k<jc.getItemCount();k++) {
            					String item = (String)jc.getItemAt(k);
            					if(FileInfoBase.getUnitsOfMeasureStr(units[0]).equals(item)) {
            						jc.setSelectedIndex(k);
            						//jc.setEnabled(false);
            					}
            				}
            			
            		}else if(l.equals("image_unit2")) {

            				JComboBox jc = (JComboBox)components.get(i);
            				for(int k=0;k<jc.getItemCount();k++) {
            					String item = (String)jc.getItemAt(k);
            					if(FileInfoBase.getUnitsOfMeasureStr(units[1]).equals(item)) {
            						jc.setSelectedIndex(k);
            						//jc.setEnabled(false);
            					}
            				}
            			
            		}else if(l.equals("image_unit3")) {

                				JComboBox jc = (JComboBox)components.get(i);
                				for(int k=0;k<jc.getItemCount();k++) {
                					String item = (String)jc.getItemAt(k);
                					if(FileInfoBase.getUnitsOfMeasureStr(units[2]).equals(item)) {
                						jc.setSelectedIndex(k);
                						//jc.setEnabled(false);
                					}
                				}
                			
            			
            		}else if(l.equals("image_unit4")) {

                				JComboBox jc = (JComboBox)components.get(i);
                				for(int k=0;k<jc.getItemCount();k++) {
                					String item = (String)jc.getItemAt(k);
                					if(FileInfoBase.getUnitsOfMeasureStr(units[3]).equals(item)) {
                						jc.setSelectedIndex(k);
                						//jc.setEnabled(false);
                					}
                				}
                			

            		}else if(l.equals("image_unit5")) {
            			//for now just disabling it
            			//components.get(i).setEnabled(false);
            		}else if(l.equals("image_resolution1")) {
            			
            				((JTextField)components.get(i)).setText(String.valueOf(res[0]));
            				//components.get(i).setEnabled(false);
            			
            		}else if(l.equals("image_resolution2")) {
            			
            				((JTextField)components.get(i)).setText(String.valueOf(res[1]));
            				//components.get(i).setEnabled(false);
            			
            		}else if(l.equals("image_resolution3")) {

                				((JTextField)components.get(i)).setText(String.valueOf(res[2]));
                				//components.get(i).setEnabled(false);
                			
            		}else if(l.equals("image_resolution4")) {
                		((JTextField)components.get(i)).setText(String.valueOf(res[3]));
                		//components.get(i).setEnabled(false);

            		}else if(l.equals("image_resolution5")) {
            			//for now just disabling it
            			//components.get(i).setEnabled(false);
            		}else if(l.equals("image_modality")) {
            				JComboBox jc = (JComboBox)components.get(i);
            				for(int k=0;k<jc.getItemCount();k++) {
            					String item = (String)jc.getItemAt(k);
            					if(modalityString.equals(item)) {
            						jc.setSelectedIndex(k);
            						//jc.setEnabled(false);
            					}
            				}

            		}else if(l.equals("image_slice_thickness")) {
            				if(sliceThickness == 0) {
            					((JTextField)components.get(i)).setText("");
            				}else {
            					((JTextField)components.get(i)).setText(String.valueOf(sliceThickness));
            				}
            				
            				//components.get(i).setEnabled(false);
            			
            		}else if(l.equals("image_orientation")) {
            				JComboBox jc = (JComboBox)components.get(i);
            				for(int k=0;k<jc.getItemCount();k++) {
            					String item = (String)jc.getItemAt(k);
            					if(orientation.equals(item)) {
            						jc.setSelectedIndex(k);
            						//jc.setEnabled(false);
            					}
            				}

            		}
            		
            	}
            	

            
    	}
    	
    	
    	/**
    	 *  action performed
    	 */
    	public void actionPerformed(ActionEvent e) {
			String command = e.getActionCommand();
			ArrayList<String> errs;
			StringBuffer errors = new StringBuffer();;
			if(command.equals("ok3")) {
				errs = validateFields();
				if(errs.size() == 0) {
					complete();
					dispose();
				}else {
					for(int i=0;i<errs.size();i++) {
						errors.append(" - " + errs.get(i) + "\n");
					}
					MipavUtil.displayError("Please correct the following errors: \n" + errors.toString());
				}
			}
			
		}
    	
    	/**
    	 * validates fields
    	 * @return
    	 */
    	public ArrayList validateFields() {
    		ArrayList<String> errs = new ArrayList<String>();
    		for(int i=0;i<xmlDataStructs.size();i++) {
	        	DataStruct ds = xmlDataStructs.get(i);
	        	String n = ds.getName();
	        	String v = ds.getVersion();

	        	parseXMLDataStructs(ds,file,errs);

	        	
	        }
    		
    		
    		return errs;
    		
    	}
    	
    	/**
    	 * validates fields
    	 * @param ds2
    	 * @param imageFile
    	 * @param errs
    	 */
    	public void parseXMLDataStructs(DataStruct ds2,File imageFile, ArrayList<String> errs) {
    		Vector<XMLAttributes> attr;
    		XMLAttributes xmlAttributes;
    		LinkedHashMap<String,String> infoMap;
    		String value = "";
    		String key = "";
    		String required = "";
    		String valuerange = "";
    		String type = "";
    		String size = "";
    		boolean found = false;
    		for(int k=0;k<ds2.size();k++) {
    			
    			Object o1 = ds2.get(k);
    			if(o1 instanceof DataElement) {
    				//data element
    				DataElement de = (DataElement)o1;
    				String name = de.getName();

    				//need to get appropriat value
    				for(int i=0;i<labels.size();i++) {
    	    			key = labels.get(i).getText();
    					if(components.get(i) instanceof JTextField) {
    						value = ((JTextField)components.get(i)).getText().trim();
    					}else if(components.get(i) instanceof JComboBox) {
    						value = (String)(((JComboBox)components.get(i)).getSelectedItem());
    					}
    					if(key.equals(name)) {
    						found = true;
    						break;
    					}
    	    		}
    				
    				
    				if(found) {
	    				//now we need to validate
	    				required = de.getRequired();
	    				type = de.getType();
	    				size = de.getSize();
	    				valuerange = de.getValuerange();
	    				if(required.equals("Required")) {
	    					if(value.trim().equals("")) {
	    						errs.add(key + " is a required field");
	    					}else {
	    						if(key.equals("image_subject_id")) {
	    							if(!value.trim().startsWith("NDAR")) {
	    								errs.add(key + " must begin with NDAR");
	    							}
	    						}
	    					}
	    				}
	    				if(type.equals("Integer")) {
	    					if(!value.trim().equals("")) {
	    						try{
	    							int intValue = Integer.valueOf(value.trim()).intValue();
	    							if(valuerange.contains("+")) {
	    								//test int if its in valuerange
	    								int min = Integer.valueOf(valuerange.substring(0, valuerange.indexOf("+")).trim()).intValue();
	    								if(min == 0) {
	    									if(intValue <= min) {
	    										errs.add(key + " must be greater than 0");
	    									}
	    								}else {
	    									if(intValue < min) {
	    										errs.add(key + " must be greater than " + min);
	    									}
	    								}
	    								
	    							}else if(valuerange.contains(" to ")) {
	    								int min = Integer.valueOf(valuerange.substring(0, valuerange.indexOf(" to ")).trim()).intValue();
	    								int max = Integer.valueOf(valuerange.substring(valuerange.indexOf(" to ") + 4, valuerange.length()).trim()).intValue();
	    								if(intValue < min  || intValue > max) {
	    									errs.add(key + " must be in the range of " + min + " to " + max);
	    								}
	    							}
	    						}catch(NumberFormatException e) {
	    							errs.add(key + " must be an Integer");
	    						}
	    					}
	    				}
	    				
	    				if(type.equals("Float")) {
	    					if(!value.trim().equals("")) {
	    						try{
	    							float floatValue = Float.valueOf(value.trim()).floatValue();
	    							if(valuerange.contains("+")) {
	    								//test int if its in valuerange
	    								float min = Float.valueOf(valuerange.substring(0, valuerange.indexOf("+")).trim()).floatValue();
	    								if(min == 0) {
	    									if(floatValue <= min) {
	    										errs.add(key + " must be greater than 0");
	    									}
	    								}else {
	    									if(floatValue < min) {
	    										errs.add(key + " must be greater than " + min);
	    									}
	    								}
	    								
	    							}else if(valuerange.contains(" to ")) {
	    								float min = Float.valueOf(valuerange.substring(0, valuerange.indexOf(" to ")).trim()).floatValue();
	    								float max = Float.valueOf(valuerange.substring(valuerange.indexOf(" to ") + 4, valuerange.length()).trim()).floatValue();
	    								if(floatValue < min  || floatValue > max) {
	    									errs.add(key + " must be in the range of " + min + " to " + max);
	    								}
	    							}
	    							
	    						}catch(NumberFormatException e) {
	    							errs.add(key + " must be an Float");
	    						}
	    					}
	    				}
	    				if(!size.equals("")) {
	    					int intValue = Integer.valueOf(size.trim()).intValue();
	    					if(!value.trim().equals("")) {
	    						if(value.length() > intValue) {
	    							errs.add(key + " must not exceed " + intValue + " in length");
	    						}
	    					}
	    					
	    				}
	    				
	    				
	    				found = false;
    				}
    				
    				
    		
    				

    			}else {
    				DataStruct ds3 = (DataStruct)o1;
    				String n = ds3.getName();
    				String v = ds3.getVersion();
    				parseXMLDataStructs(ds3, imageFile, errs);

    			}
    			
    			
    		}
    	}
    	
    	
    	public void complete() {
    		String value = "";
			for(int i=0;i<labels.size();i++) {
				if(labels.get(i).getText().equals("image_subject_id")) {
					guid = ((JTextField)components.get(i)).getText().trim();
					outputFileNameBaseTable.put(file, guid);
				}
				String key = labels.get(i).getText();
				if(components.get(i) instanceof JTextField) {
					value = ((JTextField)components.get(i)).getText().trim();
				}else if(components.get(i) instanceof JComboBox) {
					value = (String)(((JComboBox)components.get(i)).getSelectedItem());
				}
				infoMap.put(key, value);
			}
			infoTable.put(file, infoMap);
    	}

		public void windowActivated(WindowEvent e) {
			// TODO Auto-generated method stub
			
		}

		public void windowClosed(WindowEvent e) {
			// TODO Auto-generated method stub
			
		}

		public void windowClosing(WindowEvent e) {
			owner.dispose();
			
		}

		public void windowDeactivated(WindowEvent e) {
			// TODO Auto-generated method stub
			
		}

		public void windowDeiconified(WindowEvent e) {
			// TODO Auto-generated method stub
			
		}

		public void windowIconified(WindowEvent e) {
			// TODO Auto-generated method stub
			
		}

		public void windowOpened(WindowEvent e) {
			// TODO Auto-generated method stub
			
		}

    	
    	

		

    }
    
    
    
    //inner class
    /**
     * represents the DataStructure of the xml
     */
    public class DataStruct extends Vector {
		private String name;
		private String shortname;
		private String desc;
		private String version;
		private String type;
		
		public DataStruct(String name, String version) {
			super();
			this.name = name;
			this.version = version;
		}
		
		public DataStruct(String name, String shortname, String desc, String version,  String type) {
			super();
			this.name = name;
			this.shortname = shortname;
			this.desc = desc;
			this.version = version;
			this.shortname = shortname;
			this.type = type;
		}
		
		public String getName() {
			return name;
		}
		
		public String getVersion() {
			return version;
		}
		
		public String getShortname() {
			return shortname;
		}
		
		public String getType() {
			return type;
		}

		public String getDesc() {
			return desc;
		}
		
		
	}
    
    
    /**
     * represents the DataElement of the XML
     * @author pandyan
     *
     */
    public class DataElement {
    	private String name;
    	private String desc;
    	private String shortDesc;
    	private String type;
    	private String size;
    	private String required;
    	private String valuerange;
    	
    	public DataElement(String name, String desc, String shortDesc, String type, String size, String required, String valuerange) {
    		this.name = name;
    		this.desc = desc;
    		this.shortDesc = shortDesc;
    		this.type = type;
    		this.size = size;
    		this.required = required;
    		this.valuerange = valuerange;
    	}

		public String getName() {
			return name;
		}

		public String getType() {
			return type;
		}

		public String getSize() {
			return size;
		}

		public String getRequired() {
			return required;
		}
		
		public String getValuerange() {
			return valuerange;
		}

		public String getDesc() {
			return desc;
		}

		public String getShortDesc() {
			return shortDesc;
		}
		
		
    	
    	
    	
    	
    	
    }
    
    /**
     * Class used to store an xml tag's attribute (name and value)
     *
     */
    public class XMLAttributes {
    	
    	private String name;
    	private String value;
    	
    	public XMLAttributes(String n, String v) {
    		name = n;
    		value = v;
    	}
    	
    	public String getName() {
    		return name;
    	}
    	public String getValue() {
    		return value;
    	}
    	
    }
    
    
}
