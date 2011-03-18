package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmKMeans;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

public class JDialogKMeans extends JDialogScriptableBase implements AlgorithmInterface {
	
	private static final int RANDOM_INIT = 0;
	
	private static final int BRADLEY_FAYYAD_INIT = 1;
	
	private static final int HIERARCHICAL_GROUPING_INIT = 2;
	
	private static final int MAXMIN_INIT = 3;
	
	/** source image. **/
    private ModelImage image;
    
    /** result image **/
    private ModelImage resultImage;
    
    /** handle to algorithm **/
    private AlgorithmKMeans alg;
      
    /** boolean isMultifile **/
    private boolean isMultifile;
    
    private int nDims;
    
    private int extents[];
    
    // Take resolutions from the image
    // Use 1.0 in every dimension if not scaled.
    // Subscript goes from 0 to nDims - 1
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
    private int pos[][];
    
    // First subscript x = 0, y = 1, z = 2, t = 3
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
    
    private ButtonGroup initGroup;
    
    private JRadioButton randomInit;
    
    private JRadioButton BradleyInit;
    
    private JRadioButton hierarchicalInit;
    
    private JRadioButton maxMinInit;
    
    private int initSelection = RANDOM_INIT;
	
	
	public JDialogKMeans() {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		init();
	}
	
	
	
	/**
	 *  action performed
	 */
	public void actionPerformed(ActionEvent event) {
		int dimPt;
		float buffer[];
		int length;
		int i;
		int x, y, z, t;
		int xDim, yDim, zDim, tDim;
		int sliceSize;
		int volume;
		int index;
		int nval;
		String fileNameBase = null;
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
	            MipavUtil.showHelp("");
	     } else if (command.equals("AddImageBrowse")) {
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
         		if (image.isColorImage()) {
         		    MipavUtil.displayError("Image cannot be a color image");
         		    image.disposeLocal();
         		    image = null;
         		    return;
         		}
         		else if (image.isComplexImage()) {
         			MipavUtil.displayError("Image cannot be a complex image");
         		    image.disposeLocal();
         		    image = null;
         		    return;	
         		}
	         } 
	         nDims = image.getNDims();
	         scale = new double[nDims];
	         for (i = 0; i < nDims; i++) {
	        	 scale[i] = image.getFileInfo()[0].getResolutions()[i];
	         }
	         extents = image.getExtents();
	         length = extents[0];
	         for (i = 1; i < nDims; i++) {
	        	 length = length * extents[i];
	         }
	         buffer = new float[length];
	         try {
	        	 image.exportData(0, length, buffer);
	         }
	         catch (IOException e) {
	        	 MipavUtil.displayError("IOException " + e + " on image.exportData(0, length, buffer)");
	        	 image.disposeLocal();
      		     image = null;
      		     return;	
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
                 return;	 
	         }
             textImage.setText(image.getImageFileName());
	         groupNum = new int[nPoints];
             pos = new int[nDims][nPoints];
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
	         havePoints = true;
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
	                	resultsFileName = directoryPoints + fileNamePoints + "_kmeans.txt";
	                    
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
	                    pos = new int[nDims][nPoints];
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
	                    	while (start < line.length()) {
		                    	for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
		                        end = start;
		                        for (; ((end < line.length()) && ((Character.isDigit(line.charAt(end))))); end++);
		                        if (start == end) {
		                            continue l3;
		                        }
		                         pos[dimPt][nval] = Integer.valueOf(line.substring(start, end)).intValue();
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
	                MipavUtil.displayError("Out of memory in JDialogKMeans.");

	                return;
	            } catch (IOException e) {
	            	MipavUtil.displayError("IOException on BufferedReader");
	            	return;
	            }

	    }
	     

	}
	
	/**
	 *  call algorithm
	 */
	protected void callAlgorithm() {
		int i, j;
		int length;
		String fileNameBase = null;
		
		

		if ((nDims >= 2) && (nDims <= 4)  && (image == null)) {
			i = fileNamePoints.indexOf(".");
			if (i > 0) {
				fileNameBase = fileNamePoints.substring(0,i);
			}
			else {
				fileNameBase = new String(fileNamePoints);
			}
		    image = new ModelImage(ModelStorageBase.BYTE, extents, 
		    		                     makeImageName(fileNameBase, "_kmeans"));
		    length = extents[0];
		    for (i = 1; i < nDims; i++) {
		    	length = length * extents[i];
		    }
		    for (i = 0; i < length; i++) {
		    	for (j = 0; j < nDims; j++) {
		    		image.getFileInfo()[i].setResolutions((float)scale[j],j);
		    	}
		    }
		
		    try {
                new ViewJFrameImage(image, null, new Dimension(610, 240));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
		}
		centroidPos = new double[nDims][numberClusters];
		
		
		 try {
		
			 alg = new AlgorithmKMeans(image,pos,scale,groupNum,centroidPos,resultsFileName,
					                   initSelection);
			 
			 
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
             MipavUtil.displayError("Dialog KMeans: unable to allocate enough memory");

             return;
         }
		
		
		
	}
	
	
	/**
	 *  algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if (algorithm instanceof AlgorithmKMeans) {

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
        setTitle("K-Means Clustering");
        
        JLabel choiceLabel = new JLabel("Choose an image or a points file");
        choiceLabel.setForeground(Color.black);
        choiceLabel.setFont(serif12);
        mainPanel.add(choiceLabel, gbc);
        
        buttonImage = new JButton("Choose an image with points");
        buttonImage.setForeground(Color.black);
        buttonImage.setFont(serif12B);
        buttonImage.addActionListener(this);
        buttonImage.setActionCommand("AddImageBrowse");
        buttonImage.setPreferredSize(new Dimension(235, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(buttonImage, gbc);

        textImage = new JTextField();
        textImage.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textImage, gbc);
        
        buttonPointsFile = new JButton("Open a file of point locations");
        buttonPointsFile.setForeground(Color.black);
        buttonPointsFile.setFont(serif12B);
        buttonPointsFile.addActionListener(this);
        buttonPointsFile.setActionCommand("PointsFile");
        buttonPointsFile.setPreferredSize(new Dimension(225, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(buttonPointsFile, gbc);
        
        textPointsFile = new JTextField();
        textPointsFile.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textPointsFile, gbc);
        
        JLabel clustersLabel = new JLabel("Choose the number of clusters");
        clustersLabel.setForeground(Color.black);
        clustersLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(clustersLabel, gbc);
        
        textClusters = new JTextField(10);
        textClusters.setText("3");
        textClusters.setForeground(Color.black);
        textClusters.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textClusters, gbc);
        
        JLabel initLabel = new JLabel("Choose an initialization method:");
        initLabel.setForeground(Color.black);
        initLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 4;
        mainPanel.add(initLabel, gbc);
        
        initGroup = new ButtonGroup();
        randomInit = new JRadioButton("Random selection", true);
        randomInit.setFont(serif12);
        randomInit.setForeground(Color.black);
        initGroup.add(randomInit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 5;
        mainPanel.add(randomInit, gbc);
        
        BradleyInit = new JRadioButton("Bradley-Fayyad Refinement", false);
        BradleyInit.setFont(serif12);
        BradleyInit.setForeground(Color.black);
        initGroup.add(BradleyInit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 6;
        mainPanel.add(BradleyInit, gbc);
        
        hierarchicalInit = new JRadioButton("Hierarchical grouping", false);
        hierarchicalInit.setFont(serif12);
        hierarchicalInit.setForeground(Color.black);
        initGroup.add(hierarchicalInit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 7;
        mainPanel.add(hierarchicalInit, gbc);
        
        maxMinInit = new JRadioButton("MaxMin", false);
        maxMinInit.setFont(serif12);
        maxMinInit.setForeground(Color.black);
        initGroup.add(maxMinInit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 8;
        mainPanel.add(maxMinInit, gbc);
    
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        
    }
    
    private boolean setVariables() {
    	String tmpStr;
    	if (!havePoints) {
    	    MipavUtil.displayError("Must obtain points from a text file or an image");
    	    return false;
    	}
    	
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
    	
    	if (randomInit.isSelected()) {
    		initSelection = RANDOM_INIT;
    	}
    	else if (BradleyInit.isSelected()) {
    		initSelection = BRADLEY_FAYYAD_INIT;
    	}
    	else if (hierarchicalInit.isSelected()) {
    		initSelection = HIERARCHICAL_GROUPING_INIT;
    	}
    	else if (maxMinInit.isSelected()) {
    		initSelection = MAXMIN_INIT;
    	}	
    	
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
