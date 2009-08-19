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
	
	private JTextField widthField, heightField;
	
	private float width, height;
	
	private ViewJComponentEditImage componentImage;
	
	
	
	public JDialogGenerateGrid(Frame theParentFrame, ViewJComponentEditImage componentImage) {
        super(theParentFrame, false);
        this.componentImage = componentImage;
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
        
        gbc.insets = new Insets(0, 5, 0, 5);

        paramPanel.add(widthLabel, gbc);

        gbc.gridx = 1;
        paramPanel.add(widthField, gbc);

        gbc.gridx = 2;
        paramPanel.add(heightLabel, gbc);

        gbc.gridx = 3;
        paramPanel.add(heightField, gbc);
        
        
        JPanel mainPanel = new JPanel();
        mainPanel.add(paramPanel);
        mainPanel.setBorder(buildTitledBorder(""));

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

            if ((width <= 0) || (height <= 0)) {
                MipavUtil.displayError("Values must be greater than 0");

                return false;
            }
        } catch (Exception ex) {
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
	        float max = 0;
	        
	        boolean isColor = componentImage.getActiveImage().isColorImage();
	        
	        if(isColor) {
	        	int type = componentImage.getActiveImage().getType();
	        	max = (float)ModelStorageBase.getTypeMax(type);
	        }else {
	        	max = (float)componentImage.getActiveImage().getMax();
	        }
	        
	        

	        int verticalSpacing = (int)((xDim / numVertical) * componentImage.getZoomX());
	        int horizontalSpacing = (int)((yDim / numHorizontal) * componentImage.getZoomY());

	        ModelImage im = (ModelImage)componentImage.getActiveImage().clone();
	        String name = ((ModelImage)componentImage.getActiveImage()).getImageName();
	        im.setImageName(name + "_grid", true);
	        for(int z=0;z<zDim;z++) {
	        	for(int y=0;y<yDim;y++) {
	        		for(int x=0;x<xDim;x++) {
	        			int modX  = (x+1)%verticalSpacing;
	        			int modY  = (y+1)%horizontalSpacing;
	        			
	        			if(x!=0 && modX==0) {
	        				if(isColor) {
	        					im.setC(x, y, z, 1, max);
	        					im.setC(x, y, z, 2, max);
	        					im.setC(x, y, z, 3, max);
	        				}else {
	        					im.set(x,y,z, max);
	        				}
	        			}
	        			
	        			
	        			if(y!=0 && modY==0) {
	        				if(isColor) {
	        					im.setC(x, y, z, 1, max);
	        					im.setC(x, y, z, 2, max);
	        					im.setC(x, y, z, 3, max);
	        				}else {
	        					im.set(x,y,z, max);
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
