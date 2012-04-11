package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * @author pandyan
 * Class that writes grid on image
 *
 *
 */
public class JDialogGenerateGrid extends JDialogScriptableBase implements ActionDiscovery,ScriptableActionInterface  {
	
	private String unitsStr;
	
	private JTextField widthField, heightField, intensityField;
	
	private float width, height;
	
	private ViewJComponentEditImage componentImage;
	
	private boolean isColor;
	
	private float intensity, intensityR, intensityG, intensityB;
	
	private int type;
	
	private JButton gridColorButton; //for color images
	
	private Color gridColor;
	
	private ViewJColorChooser colorChooser;
	
	private int zDim, yDim, xDim;
	
	private float resX, resY;
	
	private float numVertical, numHorizontal; 
	
	private int verticalSpacing, horizontalSpacing;
	
	private ModelImage image;
	
	private ModelImage newImage;

	
	 /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
	public JDialogGenerateGrid() {}
	
	public JDialogGenerateGrid(Frame theParentFrame, ViewJComponentEditImage componentImage) {
        super(theParentFrame, false);
        this.componentImage = componentImage;
        type = componentImage.getActiveImage().getType();
        isColor = componentImage.getActiveImage().isColorImage();
        width = componentImage.getGridSpacingX();
        height = componentImage.getGridSpacingY();
        unitsStr = (Unit.getUnitFromLegacyNum(componentImage.getActiveImage().getFileInfo()[0].getUnitsOfMeasure(0))).getAbbrev();
        
    	xDim = componentImage.getActiveImage().getExtents()[0];
        yDim = componentImage.getActiveImage().getExtents()[1];
        
        if (componentImage.getActiveImage().is3DImage())
        	zDim = componentImage.getActiveImage().getExtents()[2];
        else
        	zDim = 1;

        resX = componentImage.getActiveImage().getResolutions(0)[0];
        resY = componentImage.getActiveImage().getResolutions(0)[1];

        numVertical = (xDim * resX) / width;
        numHorizontal = (yDim * resY) / height;
        

        verticalSpacing = (int)((xDim / numVertical) * componentImage.getZoomX());
        horizontalSpacing = (int)((yDim / numHorizontal) * componentImage.getZoomY());
        
        init();
        
        
	}
	
	
	
	
	public void init() {
		setTitle("Generate grid");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.gridwidth = 1;

        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());

        JLabel widthLabel = new JLabel("width (" + unitsStr + "): ");
        widthLabel.setFont(MipavUtil.font12);

        JLabel heightLabel = new JLabel("height (" + unitsStr + "): ");
        heightLabel.setFont(MipavUtil.font12);
        
        widthField = new JTextField(Float.toString(width), 4);
        widthField.setFont(MipavUtil.font12);

        heightField = new JTextField(Float.toString(height), 4);
        heightField.setFont(MipavUtil.font12);

        MipavUtil.makeNumericsOnly(widthField, true);
        MipavUtil.makeNumericsOnly(heightField, true);
        
        
        
        
        gbc.insets = new Insets(10, 5, 10, 5);

        paramPanel.add(widthLabel, gbc);

        gbc.gridx = 1;
        paramPanel.add(widthField, gbc);

        gbc.gridx = 2;
        paramPanel.add(heightLabel, gbc);

        gbc.gridx = 3;
        paramPanel.add(heightField, gbc);
        
        JPanel intensityPanel = new JPanel();
        if(isColor) {
        	float maxR = (int)componentImage.getActiveImage().getMaxR();
        	float maxG = (int)componentImage.getActiveImage().getMaxG();
        	float maxB = (int)componentImage.getActiveImage().getMaxB();
        	int max;
        	if(maxR >= maxG) {
        		if(maxR >= maxB) {
        			max = (int)maxR;
        		}else {
        			max = (int)maxB;
        		}
        	}else {
        		if(maxG >= maxB) {
        			max = (int)maxG;
        		}else {
        			max = (int)maxB;
        		}
        	}
        	intensityR = (float)max;
        	intensityG = (float)max;
        	intensityB = (float)max;
        	JLabel colorButtonLabel = new JLabel("Grid color: ");
        	colorButtonLabel.setFont(MipavUtil.font12);
        	gridColor = new Color(max,max,max);
        	gridColorButton = new JButton();
        	gridColorButton.setBackground(gridColor);
        	gridColorButton.addActionListener(this);
        	gridColorButton.setActionCommand("gridColor");
        	gridColorButton.setToolTipText("Click to change grid color");
        	intensityPanel.add(colorButtonLabel);
            intensityPanel.add(gridColorButton);
    
        	
        }else {
        	intensityField = new JTextField(Float.toString((float)componentImage.getActiveImage().getMax()), 4);
            intensityField.setFont(MipavUtil.font12);
            JLabel intensityFieldLabel = new JLabel("grid value: ");
            intensityFieldLabel.setFont(MipavUtil.font12);
            intensityPanel.add(intensityFieldLabel);
            intensityPanel.add(intensityField);
        }
        
        
        JPanel mainPanel = new JPanel(new BorderLayout());
        intensityPanel.setBorder(buildTitledBorder(""));
        mainPanel.add(paramPanel, BorderLayout.NORTH);
        
        mainPanel.add(intensityPanel, BorderLayout.SOUTH);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        //setResizable(false);
        setVisible(true);

	}
        
        

	public void actionPerformed(ActionEvent e) {
		
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("OK")) {
			if(setVariables()) {
				writeGridOverlay();
				
			}
		}else if(command.equalsIgnoreCase("Cancel")) {
			dispose();
		}else if (command.equalsIgnoreCase("gridColor")) {
                        	
			colorChooser = new ViewJColorChooser(null, "Pick grid color", new ActionListener() { // OKAY listener
                public void actionPerformed(ActionEvent ae) {
                	gridColor = colorChooser.getColor();
                	gridColorButton.setBackground(gridColor);
                	intensityR = (float)colorChooser.getColor().getRed();
                	intensityG = (float)colorChooser.getColor().getGreen();
                	intensityB = (float)colorChooser.getColor().getBlue();
                	
                }
            }, new ActionListener() { // CANCEL listener
                public void actionPerformed(ActionEvent a) { }
            });
        }

	}
	
	/**
     * Check width and height for validity.
     *
     * @return  boolean is okay
     */
    private boolean setVariables() {

        try {
            width = Float.parseFloat(widthField.getText());
            height = Float.parseFloat(heightField.getText());
            if(!isColor) {
            	intensity = Float.parseFloat(intensityField.getText());
            	if(intensity < 0 || intensity > (float)ModelStorageBase.getTypeMax(type)) {
            		MipavUtil.displayError("Value entered is out of range");
                    return false;
            	}
            }
            if ((width <= 0) || (height <= 0)) {
                MipavUtil.displayError("Values must be greater than 0");
                return false;
            }
            
        } catch (Exception ex) {
        	MipavUtil.displayError("Value entered is invalid");
            return false;
        }


        return true;
    }
	
	
	  protected void writeGridOverlay() {

		  numVertical = (xDim * resX) / Float.parseFloat(widthField.getText());
	        numHorizontal = (yDim * resY) / Float.parseFloat(heightField.getText());
	        

	        verticalSpacing = (int)((xDim / numVertical) * componentImage.getZoomX());
	        horizontalSpacing = (int)((yDim / numHorizontal) * componentImage.getZoomY());
	        
	        
	        image = (ModelImage)componentImage.getActiveImage().clone();
	        String name = ((ModelImage)componentImage.getActiveImage()).getImageName();
	        image.setImageName(name + "_grid");
	        for(int z=0;z<zDim;z++) {
	        	for(int y=0;y<yDim;y++) {
	        		for(int x=0;x<xDim;x++) {
	        			int modX  = (x+1)%verticalSpacing;
	        			int modY  = (y+1)%horizontalSpacing;
	        
	        			if(x!=0 && modX==0) {
	        				if(isColor) {
	        					image.setC(x, y, z, 1, intensityR);
	        					image.setC(x, y, z, 2, intensityG);
	        					image.setC(x, y, z, 3, intensityB);
	        				} else if (image.isComplexImage()) {
	        					image.setComplex(x, y, z, 0, intensity);
	        					image.setComplex(x, y, z, 1, intensity);
	        				}else {
	        					image.set(x,y,z, intensity);
	        				}
	        			}
	        
	        
	        			if(y!=0 && modY==0) {
	        				if(isColor) {
	        					image.setC(x, y, z, 1, intensityR);
	        					image.setC(x, y, z, 2, intensityG);
	        					image.setC(x, y, z, 3, intensityB);
	        				} else if (image.isComplexImage()) {
	        					image.setComplex(x, y, z, 0, intensity);
	        					image.setComplex(x, y, z, 1, intensity);
	        				}else {
	        					image.set(x,y,z, intensity);
	        				}
	 
	        			}
	        

	        		}
	        	}
	        }
	        image.calcMinMax();
	        setComplete(true);
	        new ViewJFrameImage(image);
	        insertScriptLine();
	        dispose();

	    }




	@Override
	protected void callAlgorithm() {
		newImage = (ModelImage) image.clone();
		
        type = newImage.getType();
        isColor = newImage.isColorImage();
        unitsStr = (Unit.getUnitFromLegacyNum(newImage.getFileInfo()[0].getUnitsOfMeasure(0))).getAbbrev();
        
    	xDim = newImage.getExtents()[0];
        yDim = newImage.getExtents()[1];
        
        if (image.is3DImage())
        	zDim = newImage.getExtents()[2];
        else
        	zDim = 1;
        	
        

        resX = newImage.getResolutions(0)[0];
        resY = newImage.getResolutions(0)[1];

        numVertical = (xDim * resX) / width;
        numHorizontal = (yDim * resY) / height;
        
        
        
        //System.out.println(numHorizontal);
       // System.out.println(numVertical);
        

        verticalSpacing = (int)(xDim / numVertical);
        horizontalSpacing = (int)(yDim / numHorizontal);
        String name = newImage.getImageName();
        newImage.setImageName(name + "_grid");
	        for(int z=0;z<zDim;z++) {
	        	for(int y=0;y<yDim;y++) {
	        		for(int x=0;x<xDim;x++) {
	        			int modX  = (x+1)%verticalSpacing;
	        			int modY  = (y+1)%horizontalSpacing;
	        			
	        			if(x!=0 && modX==0) {
	        				if(isColor) {
	        					newImage.setC(x, y, z, 1, intensityR);
	        					newImage.setC(x, y, z, 2, intensityG);
	        					newImage.setC(x, y, z, 3, intensityB);
	        				} else if (newImage.isComplexImage()) {
	        					newImage.setComplex(x, y, z, 0, intensity);
	        					newImage.setComplex(x, y, z, 1, intensity);
	        				}else {
	        					newImage.set(x,y,z, intensity);
	        				}
	        			}
	        			
	        			
	        			if(y!=0 && modY==0) {
	        				if(isColor) {
	        					newImage.setC(x, y, z, 1, intensityR);
	        					newImage.setC(x, y, z, 2, intensityG);
	        					newImage.setC(x, y, z, 3, intensityB);
	        				} else if (newImage.isComplexImage()) {
	        					newImage.setComplex(x, y, z, 0, intensity);
	        					newImage.setComplex(x, y, z, 1, intensity);
	        				}else {
	        					newImage.set(x,y,z, intensity);
	        				}
	        				
	        			}
	        			
	        			
	        		}
	        	}
	        }
	        newImage.calcMinMax();
        setComplete(true);
	        dispose();
	        
		
	    }





	protected void setGUIFromParams() {
		image = scriptParameters.retrieveInputImage();
		parentFrame = image.getParentFrame();
		
		width = scriptParameters.getParams().getFloat("width");
		height = scriptParameters.getParams().getFloat("height");
		intensity = scriptParameters.getParams().getFloat("grid_value");
			
		
}
 

	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(image);
		scriptParameters.storeOutputImageParams(newImage, true);
		try {
	        scriptParameters.getParams().put(ParameterFactory.newParameter("width", width));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("height", height));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("grid_value", intensity));
			} catch (ParserException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		
	}
	
	
	
	protected void doPostAlgorithmActions(){
		AlgorithmParameters.storeImageInRunner(image);
	}
	
    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Utilities");
            }

            public String getDescription() {
                return new String("Adds a grid to image.");
            }

            public String getDescriptionLong() {
                return new String("Adds a grid to image.");
            }

            public String getShortLabel() {
                return new String("Grid");
            }

            public String getLabel() {
                return new String("Generate Grid");
            }

            public String getName() {
                return new String("Generate Grid");
            }
        };
    }
    
    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterFloat("width",20));
            table.put(new ParameterFloat("height",20));
            table.put(new ParameterFloat("grid_value",1798 ));
            } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }
    
    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }
    
    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
                return newImage.getImageName();

            }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }
    
    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }






}
