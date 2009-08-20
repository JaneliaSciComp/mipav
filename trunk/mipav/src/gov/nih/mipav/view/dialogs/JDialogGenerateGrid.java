package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class JDialogGenerateGrid extends JDialogBase {
	
	private String unitsStr;
	
	private JTextField widthField, heightField, intensityField, intensityRField, intensityGField, intensityBField;
	
	private float width, height;
	
	private ViewJComponentEditImage componentImage;
	
	private boolean isColor;
	
	private float intensity, intensityR, intensityG, intensityB;
	
	private int type;
	
	
	public JDialogGenerateGrid(Frame theParentFrame, ViewJComponentEditImage componentImage) {
        super(theParentFrame, false);
        this.componentImage = componentImage;
        type = componentImage.getActiveImage().getType();
        isColor = componentImage.getActiveImage().isColorImage();
        width = componentImage.getVOIHandler().getGridSpacingX();
        height = componentImage.getVOIHandler().getGridSpacingY();
        unitsStr = FileInfoBase.getUnitsOfMeasureAbbrevStr(componentImage.getActiveImage().getFileInfo()[0].getUnitsOfMeasure(0));
        init();
        
        
	}
	
	
	
	
	public void init() {
		setTitle("Generate Grid");

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
        	float maxR = (float)componentImage.getActiveImage().getMaxR();
        	float maxG = (float)componentImage.getActiveImage().getMaxG();
        	float maxB = (float)componentImage.getActiveImage().getMaxB();
        	float max;
        	if(maxR >= maxG) {
        		if(maxR >= maxB) {
        			max = maxR;
        		}else {
        			max = maxB;
        		}
        	}else {
        		if(maxG >= maxB) {
        			max = maxG;
        		}else {
        			max = maxB;
        		}
        	}
        	intensityRField = new JTextField(Float.toString(max), 4);
            intensityRField.setFont(MipavUtil.font12);
            intensityGField = new JTextField(Float.toString(max), 4);
            intensityGField.setFont(MipavUtil.font12);
            intensityBField = new JTextField(Float.toString(max), 4);
            intensityBField.setFont(MipavUtil.font12);
            JLabel intensityFieldRLabel = new JLabel("grid R value: ");
            intensityFieldRLabel.setFont(MipavUtil.font12);
            JLabel intensityFieldGLabel = new JLabel("grid G value: ");
            intensityFieldGLabel.setFont(MipavUtil.font12);
            JLabel intensityFieldBLabel = new JLabel("grid B value: ");
            intensityFieldBLabel.setFont(MipavUtil.font12);
            intensityPanel.add(intensityFieldRLabel);
            intensityPanel.add(intensityRField);
            intensityPanel.add(intensityFieldGLabel);
            intensityPanel.add(intensityGField);
            intensityPanel.add(intensityFieldBLabel);
            intensityPanel.add(intensityBField);
        	
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
            if(isColor) {
            	intensityR = Float.parseFloat(intensityRField.getText());
            	if(intensityR < 0 || intensityR > (float)ModelStorageBase.getTypeMax(type)) {
            		MipavUtil.displayError("Value entered is out of range");
                    return false;
            	}
            	intensityG = Float.parseFloat(intensityGField.getText());
            	if(intensityG < 0 || intensityG > (float)ModelStorageBase.getTypeMax(type)) {
            		MipavUtil.displayError("Value entered is out of range");
                    return false;
            	}
            	intensityB = Float.parseFloat(intensityBField.getText());
            	if(intensityB < 0 || intensityB > (float)ModelStorageBase.getTypeMax(type)) {
            		MipavUtil.displayError("Value entered is out of range");
                    return false;
            	}
            }else {
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
	    	int xDim = componentImage.getActiveImage().getExtents()[0];
	        int yDim = componentImage.getActiveImage().getExtents()[1];
	        int zDim = componentImage.getActiveImage().getExtents()[2];

	        float resX = componentImage.getActiveImage().getResolutions(0)[0];
	        float resY = componentImage.getActiveImage().getResolutions(0)[1];

	        float numVertical = (xDim * resX) / width;
	        float numHorizontal = (yDim * resY) / height;
	        
	        
	        
	 
	        

	        int verticalSpacing = (int)((xDim / numVertical) * componentImage.getZoomX());
	        int horizontalSpacing = (int)((yDim / numHorizontal) * componentImage.getZoomY());

	        ModelImage im = (ModelImage)componentImage.getActiveImage().clone();
	        String name = ((ModelImage)componentImage.getActiveImage()).getImageName();
	        im.setImageName(name + "_grid");
	        for(int z=0;z<zDim;z++) {
	        	for(int y=0;y<yDim;y++) {
	        		for(int x=0;x<xDim;x++) {
	        			int modX  = (x+1)%verticalSpacing;
	        			int modY  = (y+1)%horizontalSpacing;
	        			
	        			if(x!=0 && modX==0) {
	        				if(isColor) {
	        					im.setC(x, y, z, 1, intensityR);
	        					im.setC(x, y, z, 2, intensityG);
	        					im.setC(x, y, z, 3, intensityB);
	        				}else {
	        					im.set(x,y,z, intensity);
	        				}
	        			}
	        			
	        			
	        			if(y!=0 && modY==0) {
	        				if(isColor) {
	        					im.setC(x, y, z, 1, intensityR);
	        					im.setC(x, y, z, 2, intensityG);
	        					im.setC(x, y, z, 3, intensityB);
	        				}else {
	        					im.set(x,y,z, intensity);
	        				}
	        				
	        			}
	        			
	        			
	        		}
	        	}
	        }
	        im.calcMinMax();
	        new ViewJFrameImage(im);
	        dispose();
	        
	    }

}
