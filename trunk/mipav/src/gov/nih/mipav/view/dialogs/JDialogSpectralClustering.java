package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmSpectralClustering;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Enumeration;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class JDialogSpectralClustering extends JDialogScriptableBase implements AlgorithmInterface {
	
	/** source image. **/
    private ModelImage image;
    
    /** result image **/
    private ModelImage resultImage;
    
    /** handle to algorithm **/
    private AlgorithmSpectralClustering alg;
      
    /** boolean isMultifile **/
    private boolean isMultifile;
    
    private int nDims;
    
    private int extents[];
    
    // Take resolutions from any black and white image
    // Use 1.0 in every dimension if not scaled or if color image.
    // Subscript goes from 0 to nDims - 1 for black and white
    // and from 0 to 1 for color.
    private double scale[];
    
    private String directoryPoints;
    
    private String fileNamePoints;
    
    private File filePoints;
    
    private BufferedReader br;
    
    private int nPoints;
    
    // subscript goes from 0 to nPoints-1 for each point
    // Value is the cluster number from 0 to numberClusters-1.
    private int groupNum[];
    
    // First subscript x = 0, y = 1, z = 2, t = 3
    // Second subscript 0 to nPoints-1 for each point
    // Value is the point position
    private double pos[][];
    
    // subscript goes from 0 to nPoints-1 for each point
    // Value is the weight or number of occurrences of each point
    private double weight[];
    
    // First subscript is number of dimensions used in spectral clustering = numberClusters
    // Second subscript 0 to numberClusters-1 for each cluster
    // Value is the cluster position
    private double centroidPos[][];
    
    private JTextField textImage;
    
    private JButton buttonImage;
    
    private JTextField textPointsFile;
    
    private JButton buttonPointsFile;
    
    private JTextField textClusters;
    
    private int numberClusters;
    
    private boolean havePoints = false;
    
    private String resultsFileName = null;
    
    private float redBuffer[] = null;
	private float greenBuffer[] = null;
	private float blueBuffer[] = null;
	// Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
    private double scaleMax = 255.0;
    
    private JCheckBox unitVarianceCheckBox;
    
    private boolean scaleVariablesToUnitVariance;
    
    private JLabel resultsFileNameLabel;
    
    private JTextField resultsFileNameText;

	private JComboBox imageList;
	
	
	public JDialogSpectralClustering() {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		init();
	}
	
	
	
	/**
	 *  action performed
	 */
	public void actionPerformed(ActionEvent event) {
		int dimPt;
		int i;
		
		int nval;

		String fileNameBase = null;
		//boolean readWeight;
		//Object source = event.getSource();
		String command = event.getActionCommand();
		 if (command.equals("OK")) {
			 if (setVariables()) {
			     callAlgorithm();
			 }
	     } 
		 else if (command.equals("Cancel")) {
	    	 if (image != null) {
	    		 image.disposeLocal();
	    		 image = null;
	    	 }
	         dispose();
	     } else if (command.equals("Help")) {
	            //MipavUtil.showHelp("");
	     } else if (command.equals("PointFile")) {

	            try {
	                JFileChooser chooser = new JFileChooser();

	                if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
	                    File file = new File(ViewUserInterface.getReference().getDefaultDirectory());

	                    if (file != null) {
	                        chooser.setCurrentDirectory(file);
	                    } else {
	                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	                    }
	                } else {
	                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	                }

	                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
	                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
	                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
	                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

	                chooser.setDialogTitle("Open file of point locations");
	                directoryPoints = String.valueOf(chooser.getCurrentDirectory()) + File.separator;

	                int returnValue = chooser.showOpenDialog(ViewUserInterface.getReference().getMainFrame());

	                if (returnValue == JFileChooser.APPROVE_OPTION) {
	                    fileNamePoints = chooser.getSelectedFile().getName();
	                    directoryPoints = String.valueOf(chooser.getCurrentDirectory()) + File.separator;
	                    ViewUserInterface.getReference().setDefaultDirectory(directoryPoints);
	                } else {
	                    fileNamePoints = null;

	                    return;
	                }

	                if (fileNamePoints != null) {
	                	filePoints = new File(directoryPoints + fileNamePoints);
	                	i = fileNamePoints.indexOf(".");
	    				if (i > 0) {
	    					fileNameBase = chooser.getSelectedFile().getName().substring(0,i);
	    				}
	    				else {
	    					fileNameBase = new String(chooser.getSelectedFile().getName());
	    				}
	                	resultsFileName = directoryPoints + fileNameBase + "_spectral_clustering.txt";
	                	resultsFileNameLabel.setEnabled(true);
	                	resultsFileNameText.setEnabled(true);
	                	resultsFileNameText.setText(resultsFileName);
	                    
	                    try {
	                        br = new BufferedReader(new InputStreamReader(new FileInputStream(filePoints)));
	                    }
	                    catch (FileNotFoundException e) {
	                        MipavUtil.displayError((directoryPoints + fileNamePoints) + " was not found");
	                        return;
	                    }
	                    
	                    // Read lines until first character is not blank and not #
	                    int ii = 0;
	                    String line = null;
	                    do {
	                        try {
	                            // Contains the contents of the line not including line termination characters
	                            line = br.readLine(); 
	                        }
	                        catch(IOException e) {
	                            MipavUtil.displayError("IOException on br.readLine");
	                            br.close();
	                            return;
	                        }
	                        // have reached end of stream
	                        if (line == null) {
	                            MipavUtil.displayError("Have reached end of stream on br.readLine");
	                            br.close();
	                            return;
	                        }
	                        for (ii = 0; ((ii < line.length()) && (Character.isSpaceChar(line.charAt(ii)))); ii++);
	                    } while ((ii == line.length()) || (line.charAt(ii) == '#'));
	                    
	                    int start = 0;
	                    int end = 0;
	                    for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
	                    end = start;
	                    for (; ((end < line.length()) && (Character.isDigit(line.charAt(end)))); end++);
	                    if (start == end) {
	                        MipavUtil.displayError("No digit starts line which should contain number of dimensions");
	                        br.close();
	                        return;
	                    }
	                    nDims = Integer.valueOf(line.substring(start, end)).intValue();
	                    extents = new int[nDims];
	                    scale = new double[nDims];
	                    nval = 0;
	                   l1: while (true) {
	                    	try {
	                            // Contains the contents of the line not including line termination characters
	                            line = br.readLine();  
	                        }
	                        catch(IOException e) {
	                            MipavUtil.displayError("IOException on br.readLine");
	                            br.close();
	                            return;
	                        }
	                        // have reached end of stream
	                        if (line == null) {
	                            MipavUtil.displayError("Have reached end of stream on br.readLine");
	                            break;
	                        }
	                    	start = 0;
	                    	end = 0;
	                    	while (start < line.length()) {
		                    	for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
		                        end = start;
		                        for (; ((end < line.length()) && ((Character.isDigit(line.charAt(end))))); end++);
		                        if (start == end) {
		                            continue l1;
		                        }
		                        extents[nval++] = Integer.valueOf(line.substring(start, end)).intValue();
		                        if (nval ==  nDims) {
		                            break l1;
		                        }
		                        start = end;
	                    	} // while (start < line.length())
	                    } // while (true)
	                    if (nval < 1) {
	                        MipavUtil.displayError("No extent values found in " + fileNamePoints);
	                        return;
	                    }
	                    if (nval < nDims) {
	                    	MipavUtil.displayError("Only " + nval + " of " + nDims + " required dimension lengths found");
	                    	return;
	                    }
	                    start = 0;
	                    end = 0;
	                    nval = 0;
	                   l2: while (true) {
	                    	try {
	                            // Contains the contents of the line not including line termination characters
	                            line = br.readLine();  
	                        }
	                        catch(IOException e) {
	                            MipavUtil.displayError("IOException on br.readLine");
	                            br.close();
	                            return;
	                        }
	                        // have reached end of stream
	                        if (line == null) {
	                            MipavUtil.displayError("Have reached end of stream on br.readLine");
	                            break;
	                        }
	                    	start = 0;
	                    	end = 0;
	                    	while (start < line.length()) {
		                    	for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
		                        end = start;
		                        for (; ((end < line.length()) && ((Character.isDigit(line.charAt(end))) || (line.charAt(end) == '.') || 
		                                       (line.charAt(end) == 'e') || (line.charAt(end) == 'E') ||
		                                       (line.charAt(end) == '+') || (line.charAt(end) == '-'))); end++);
		                        if (start == end) {
		                            continue l2;
		                        }
		                        scale[nval++] = Double.valueOf(line.substring(start, end)).doubleValue();
		                        if (nval ==  nDims) {
		                            break l2;
		                        }
		                        start = end;
	                    	} // while (start < line.length())
	                    } // while (true)
	                    if (nval < 1) {
	                        MipavUtil.displayError("No scale values found in " + fileNamePoints);
	                        return;
	                    }
	                    if (nval < nDims) {
	                    	MipavUtil.displayError("Only " + nval + " of " + nDims + " required scale values found");
	                    	return;
	                    }
	                    // Read lines until first character is not blank and not #
	                    ii = 0;
	                    line = null;
	                    do {
	                        try {
	                            // Contains the contents of the line not including line termination characters
	                            line = br.readLine(); 
	                        }
	                        catch(IOException e) {
	                            MipavUtil.displayError("IOException on br.readLine");
	                            br.close();
	                            return;
	                        }
	                        // have reached end of stream
	                        if (line == null) {
	                            MipavUtil.displayError("Have reached end of stream on br.readLine");
	                            br.close();
	                            return;
	                        }
	                        for (ii = 0; ((ii < line.length()) && (Character.isSpaceChar(line.charAt(ii)))); ii++);
	                    } while ((ii == line.length()) || (line.charAt(ii) == '#'));
	                    start = 0;
	                    end = 0;
	                    for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
	                    end = start;
	                    for (; ((end < line.length()) && (Character.isDigit(line.charAt(end)))); end++);
	                    if (start == end) {
	                        MipavUtil.displayError("No digit starts line which should contain number of points");
	                        br.close();
	                        return;
	                    }
	                    nPoints = Integer.valueOf(line.substring(start, end)).intValue();
	                    groupNum = new int[nPoints];
	                    pos = new double[nDims][nPoints];
	                    //weight = new double[nPoints];
	                    nval = 0;
	                    dimPt = 0;
	                    l3: while (true) {
	                    	try {
	                            // Contains the contents of the line not including line termination characters
	                            line = br.readLine();  
	                        }
	                        catch(IOException e) {
	                            MipavUtil.displayError("IOException on br.readLine");
	                            br.close();
	                            return;
	                        }
	                        // have reached end of stream
	                        if (line == null) {
	                            MipavUtil.displayError("Have reached end of stream on br.readLine");
	                            break;
	                        }
	                    	start = 0;
	                    	end = 0;
	                    	//readWeight = false;
	                    	while (start < line.length()) {
		                    	for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
		                        end = start;
		                        for (; ((end < line.length()) && ((Character.isDigit(line.charAt(end))) || (line.charAt(end) == '.') || 
	                                       (line.charAt(end) == 'e') || (line.charAt(end) == 'E') ||
	                                       (line.charAt(end) == '+') || (line.charAt(end) == '-'))); end++);
		                        if (start == end) {
		                            continue l3;
		                        }
		                        /*if (!readWeight) {
		                        	readWeight = true;
		                        	weight[nval] = Double.valueOf(line.substring(start, end)).doubleValue();
		                        	start = end;
		                        	continue;
		                        }*/
		                         pos[dimPt][nval] = Double.valueOf(line.substring(start, end)).doubleValue();
		                        if (dimPt == nDims-1) {
		                        	nval++;
		                        }
		                        if (dimPt < nDims-1) {
		                        	dimPt++;
		                        }
		                        else {
		                        	dimPt = 0;
		                        }
		                        if (nval ==  nPoints) {
		                            break l3;
		                        }
		                        start = end;
	                    	} // while (start < line.length())
	                    } // while (true)
	                    br.close();
	                    if (nval < 1) {
	                        MipavUtil.displayError("No set of point values found in " + fileNamePoints);
	                        return;
	                    }
	                    if (nval < nPoints) {
	                    	MipavUtil.displayError("Only " + nval + " of " + nPoints + " required points found");
	                    	return;
	                    }
                        havePoints = true;
	                	textPointsFile.setText(fileNamePoints);
	                }
	            } catch (OutOfMemoryError e) {
	                MipavUtil.displayError("Out of memory in JDialogSpectralClustering.");

	                return;
	            } catch (IOException e) {
	            	MipavUtil.displayError("IOException on BufferedReader");
	            	return;
	            }

	    } else {
            super.actionPerformed(event);
        }
	     

	}
	
	/**
	 *  call algorithm
	 */
	protected void callAlgorithm() {
		int i, j;
		int length;
		String fileNameBase = null;
		
		

		centroidPos = new double[numberClusters][numberClusters];
		if (((nDims >= 2) && (nDims <= 3)  && (image == null)) || ((image != null) && (image.isColorImage()))) {
			if ((image != null) && (image.isColorImage())) {
				scaleMax = Math.max(scaleMax, image.getMax());
				image = new ModelImage(ModelStorageBase.BYTE, extents, 
	                     makeImageName(image.getImageFileName(), "_kmeans"));
				
				length = 1;
			    for (i = 2; i < nDims; i++) {
			    	length = length * extents[i];
			    }
			    for (i = 0; i < length; i++) {
			        image.getFileInfo()[i].setResolutions(image.getFileInfo()[0].getResolutions());
			    }
			}
			else {
				i = fileNamePoints.indexOf(".");
				if (i > 0) {
					fileNameBase = fileNamePoints.substring(0,i);
				}
				else {
					fileNameBase = new String(fileNamePoints);
				}
			    image = new ModelImage(ModelStorageBase.BYTE, extents, 
			    		                     makeImageName(fileNameBase, "_kmeans"));
			    
			    length = 1;
			    for (i = 2; i < nDims; i++) {
			    	length = length * extents[i];
			    }
			    for (i = 0; i < length; i++) {
			    	for (j = 0; j < nDims; j++) {
			    		image.getFileInfo()[i].setResolutions((float)scale[j],j);
			    	}
			    }
			} // else 
		    
		} // if (((nDims >= 2) && (nDims <= 3)  && (image == null)) || ((image != null) && (image.isColorImage())))
		
		
		 try {
		
			 alg = new AlgorithmSpectralClustering(image, pos, scale, groupNum, centroidPos, resultsFileName, redBuffer,
			                                       greenBuffer, blueBuffer, scaleMax, scaleVariablesToUnitVariance);
			 
			 
			 //This is very important. Adding this object as a listener allows the algorithm to
             // notify this object when it has completed of failed. See algorithm performed event.
             // This is made possible by implementing AlgorithmedPerformed interface
             alg.addListener(this);
             
             if (image != null) {
                 createProgressBar(image.getImageName(), alg);
             }
             else {
            	 createProgressBar(fileNameBase + "_kmeans", alg);
             }


             // Hide dialog
             setVisible(false);

             if (isRunInSeparateThread()) {

                 // Start the thread as a low priority because we wish to still have user interface work fast.
                 if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                     MipavUtil.displayError("A thread is already running on this object");
                 }
             } else {
                 alg.run();
             }
         } catch (OutOfMemoryError x) {

             if (resultImage != null) {
                 resultImage.disposeLocal(); // Clean up memory of result image
                 resultImage = null;
             }

             System.gc();
             MipavUtil.displayError("Dialog Spectral Clustering: unable to allocate enough memory");

             return;
         }
		
		
		
	}
	
	private void loadImage() {
    	String fileNameBase;
    	int i;
		ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
         JFileChooser chooser = fileChooser.getFileChooser();
         if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }
         chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
         chooser.setDialogTitle("Choose image");
         int returnValue = chooser.showOpenDialog(this);
         if (returnValue == JFileChooser.APPROVE_OPTION) { 	
         	FileIO fileIO = new FileIO();
         	isMultifile = fileChooser.isMulti();
         	image = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator,
         			                 isMultifile, null);
         	i = chooser.getSelectedFile().getName().indexOf(".");
			if (i > 0) {
				fileNameBase = chooser.getSelectedFile().getName().substring(0,i);
			}
			else {
				fileNameBase = new String(chooser.getSelectedFile().getName());
			}
         	resultsFileName = chooser.getCurrentDirectory() + File.separator + fileNameBase + "_kmeans.txt";
         	resultsFileNameLabel.setEnabled(true);
         	resultsFileNameText.setEnabled(true);
         	resultsFileNameText.setText(resultsFileName);
    		if (image.isComplexImage()) {
    			MipavUtil.displayError("Image cannot be a complex image");
    		    image.disposeLocal();
    		    image = null;
    		    return;	
    		}
    		
    		imageList.addItem(image.getImageName());
    		imageList.setSelectedItem(image.getImageName());
    		textImage.setText(image.getImageName());
         } 
     } 
	
	
	/**
	 *  algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if (algorithm instanceof AlgorithmSpectralClustering) {

            if ((alg.isCompleted() == true) && (image != null)) {

                
               
            }
		}
		
		if (algorithm.isCompleted()) {
            insertScriptLine();
        }
		alg.finalize();
        alg = null;
		dispose();
	}
	
	
	
	/**
	 * init
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {

    	GuiBuilder gui = new GuiBuilder(this);
    	
        JPanel mainPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridx = 0;
    	setForeground(Color.black);
        setTitle("Spectral Clustering");
        
        JLabel choiceLabel = new JLabel("Choose an image or a points file");
        choiceLabel.setForeground(Color.black);
        choiceLabel.setFont(serif12);
        mainPanel.add(choiceLabel, gbc);
        
        JLabel choiceLabel2 = new JLabel("A black and white image with only points generates centroids");
        choiceLabel2.setForeground(Color.black);
        choiceLabel2.setFont(serif12);
        gbc.gridy++;
        mainPanel.add(choiceLabel2, gbc);
        
        JLabel choiceLabel3 = new JLabel("A color or multispectral image is segmented");
        choiceLabel3.setForeground(Color.black);
        choiceLabel3.setFont(serif12);
        gbc.gridy++;
        mainPanel.add(choiceLabel3, gbc);
        
        buttonImage = new JButton("Choose an image");
        buttonImage.setForeground(Color.black);
        buttonImage.setFont(serif12B);
        buttonImage.addActionListener(this);
        buttonImage.setActionCommand("AddImageBrowse");
        buttonImage.setPreferredSize(new Dimension(235, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        
        Object[] imgList = new Object[ViewUserInterface.getReference().getRegisteredImagesNum()+1];
        imgList[0] = "Load image...";
        Enumeration<String> strEnum = ViewUserInterface.getReference().getRegisteredImageNames();
        for(int i=1; i<imgList.length; i++) {
        	imgList[i] = strEnum.nextElement();
        }
        imageList = gui.buildComboBox("Choose an image: ", imgList, 0);
        imageList.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
	            if(imageList.getSelectedIndex() == 0) {
	            	loadImage();
	            } else {
	            	textImage.setText(imageList.getSelectedItem().toString());
	            }
            }
        });
        mainPanel.add(imageList.getParent(), gbc);

        textImage = new JTextField();
        textImage.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textImage, gbc);
        
        buttonPointsFile = new JButton("Open a file of point locations");
        buttonPointsFile.setForeground(Color.black);
        buttonPointsFile.setFont(serif12B);
        buttonPointsFile.addActionListener(this);
        buttonPointsFile.setActionCommand("PointFile");
        buttonPointsFile.setPreferredSize(new Dimension(225, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(buttonPointsFile, gbc);
        
        textPointsFile = new JTextField();
        textPointsFile.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textPointsFile, gbc);
        
        unitVarianceCheckBox = new JCheckBox("Scale variables to unit variance", false);
        unitVarianceCheckBox.setFont(serif12);
        unitVarianceCheckBox.setForeground(Color.black);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(unitVarianceCheckBox, gbc);
        
        JLabel clustersLabel = new JLabel("Choose the number of clusters");
        clustersLabel.setForeground(Color.black);
        clustersLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(clustersLabel, gbc);
        
        textClusters = new JTextField(10);
        textClusters.setText("3");
        textClusters.setForeground(Color.black);
        textClusters.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textClusters, gbc);
        
        resultsFileNameLabel = new JLabel("Results file name:");
        resultsFileNameLabel.setForeground(Color.black);
        resultsFileNameLabel.setFont(serif12);
        resultsFileNameLabel.setEnabled(false);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(resultsFileNameLabel, gbc);
        
        resultsFileNameText = new JTextField(40);
        resultsFileNameText.setText("");
        resultsFileNameText.setForeground(Color.black);
        resultsFileNameText.setFont(serif12);
        resultsFileNameText.setEnabled(false);
        gbc.gridx = 1;
        mainPanel.add(resultsFileNameText, gbc);
    
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        
    }
    
    private boolean setVariables() {
    	double redMin;
		double redMax;
		double greenMin;
		double greenMax;
		double blueMin;
		double blueMax;
		
		int index;
		double buffer[];
		int length;
		
		int x, y, z, t;
		int xDim, yDim, zDim, tDim;
		int sliceSize;
		int volume;
		
		int i;
    	
		int nval;
		int dim;
		double totalSum;
		double totalSumSquared;
		double variance;
		
    	if (!havePoints) {
    		image = ViewUserInterface.getReference().getRegisteredImageByName(imageList.getSelectedItem().toString());
        	
        	if(image == null) {
        		MipavUtil.displayError("No image with name "+imageList.getSelectedItem()+" was found.");
        		return false;
        	}
        	
        	nDims = image.getNDims();
            extents = image.getExtents();
            length = extents[0];
            for (i = 1; i < nDims; i++) {
           	 length = length * extents[i];
            }
            if (image.isColorImage()) {
           	 scale = new double[2];
           	 scale[0] = 1.0;
           	 scale[1] = 1.0;
           	 redMin = image.getMinR();
           	 redMax = image.getMaxR();
           	 if (redMin != redMax) {
    	        	 redBuffer = new float[length];
    	        	 try {
    	             image.exportRGBData(1, 0, length, redBuffer); 
    	        	 }
    	        	 catch (IOException e) {
    		        	 MipavUtil.displayError("IOException " + e + " on image.exportRGBData(1, 0, length, redBuffer)");
    		        	 image.disposeLocal();
    	      		     image = null;
    	      		     return false;	
    		         }
           	 } // if (redMin != redMax)
           	 greenMin = image.getMinG();
           	 greenMax = image.getMaxG();
           	 if (greenMin != greenMax) {
    	        	 greenBuffer = new float[length];
    	        	 try {
    	             image.exportRGBData(2, 0, length, greenBuffer); 
    	        	 }
    	        	 catch (IOException e) {
    		        	 MipavUtil.displayError("IOException " + e + " on image.exportRGBData(2, 0, length, greenBuffer)");
    		        	 image.disposeLocal();
    	      		     image = null;
    	      		     return false;	
    		         }
           	 } // if (greenMin != greenMax)
           	 blueMin = image.getMinB();
           	 blueMax = image.getMaxB();
           	 if (blueMin != blueMax) {
    	        	 blueBuffer = new float[length];
    	        	 try {
    	             image.exportRGBData(3, 0, length, blueBuffer); 
    	        	 }
    	        	 catch (IOException e) {
    		        	 MipavUtil.displayError("IOException " + e + " on image.exportRGBData(3, 0, length, blueBuffer)");
    		        	 image.disposeLocal();
    	      		     image = null;
    	      		     return false;	
    		         }
           	 } // if (blueMin != blueMax)
           	 textImage.setText(image.getImageFileName());
           	 nPoints = length;
            } // if (image.isColorImage())
            else { // black and white point image
           	 scale = new double[nDims];
    	         for (i = 0; i < nDims; i++) {
    	        	 scale[i] = image.getFileInfo()[0].getResolutions()[i];
    	         }
    	         buffer = new double[length];
    	         try {
    	        	 image.exportData(0, length, buffer);
    	         }
    	         catch (IOException e) {
    	        	 MipavUtil.displayError("IOException " + e + " on image.exportData(0, length, buffer)");
    	        	 image.disposeLocal();
         		     image = null;
         		     return false;	
    	         }
    	         nPoints = 0;
    	         for (i = 0; i < length; i++) {
    	        	 if (buffer[i] > 0) {
    	        		 nPoints++;
    	        	 }
    	         }
    	         if (nPoints == 0) {
    	        	 MipavUtil.displayError("No set of point values found in " + image.getImageFileName());
    	        	 image.disposeLocal();
    	        	 image = null;
                    return false;	 
    	         }
                textImage.setText(image.getImageFileName());
    	         groupNum = new int[nPoints];
                pos = new double[nDims][nPoints];
                weight = new double[nPoints];
                if (nDims >= 4) {
               	 tDim = extents[3];
                }
                else {
               	 tDim = 1;
                }
    	         if (nDims >= 3) {
    	        	 zDim = extents[2];
    	         }
    	         else {
    	        	 zDim = 1;
    	         }
    	         if (nDims >= 2) {
    	        	 yDim = extents[1];
    	         }
    	         else {
    	        	 yDim = 1;
    	         }
    	         xDim = extents[0];
    	         sliceSize = xDim * yDim;
    	         volume = sliceSize * zDim;
    	         nval = 0;
    	         for (t = 0; t < tDim; t++) {
    	        	 for (z = 0; z < zDim; z++) {
    	        		 for (y = 0; y < yDim; y++) {
    	        			 for (x = 0; x < xDim; x++) {
    	        			     index = x + y*xDim + z*sliceSize + t*volume;
    	        			     if (buffer[index] > 0) {
    	        			    	 weight[nval] = buffer[index];
    	        			         pos[0][nval] = x;
    	        			         if (nDims >= 2) {
    	        			        	 pos[1][nval] = y;
    	        			        	 if (nDims >= 3) {
    	        			        		 pos[2][nval] = z;
    	        			        		 if (nDims >= 4) {
    	        			        			 pos[3][nval] = t;
    	        			        		 }
    	        			        	 }
    	        			         }
    	        			         nval++;
    	        			     } // if (buffer[index] > 0)
    	        			 }
    	        		 }
    	        	 }
    	         } // for (t = 0; t < tDim; t++)
    	         buffer = null;
            } // else black and white point image
            havePoints = true;
    	} // if (!havePoints)
    	
    	String tmpStr;
    	i = 0;
    	
    	if (!havePoints) {
    	    MipavUtil.displayError("Must obtain points from a text file or an image");
    	    return false;
    	}
    	
    	scaleVariablesToUnitVariance = unitVarianceCheckBox.isSelected();
        if (scaleVariablesToUnitVariance) {
            
            for (dim = 0; dim < nDims; dim++) {
                 totalSum = 0.0;
                 totalSumSquared = 0.0;
                 for (i = 0; i < nPoints; i++) {
                     totalSum += pos[dim][i];
                     totalSumSquared += pos[dim][i]*pos[dim][i];
                 }
                 variance = (totalSumSquared - totalSum*totalSum/nPoints)/(nPoints - 1.0);
                 scale[dim] = 1.0/Math.sqrt(variance);
            } // for (dim = 0; dim < nDims; dim++) 
        } // if (scaleVariablesToUnitVariance)
    	
    	tmpStr = textClusters.getText();
    	numberClusters = Integer.parseInt(tmpStr);
    	if (numberClusters < 2) {
    		MipavUtil.displayError("Must have at least 2 clusters");
    		return false;
    	}
    	if (numberClusters > nPoints) {
    		MipavUtil.displayError("The number of clusters must not exceed the number of points");
    		return false;
    	}
    	
    	resultsFileName = resultsFileNameText.getText();
    	
    	return true;
    }
    
    
    
    
	

	

	/**
	 * set GUI from params
	 */
	protected void setGUIFromParams(){
		
	}

	/**
	 * store params from gui
	 */
	protected void storeParamsFromGUI() throws ParserException {
		
	}
	
	 /**
     * get result image
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }
    
    
	
	/**
     * item staate changed
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        
    }



    
    /**
     *  windoe closing
     */
    public void windowClosing(WindowEvent event) {
        if (image != null) {
        	image.disposeLocal();
        	image = null;
        }
        dispose();
    }

	


	
}
