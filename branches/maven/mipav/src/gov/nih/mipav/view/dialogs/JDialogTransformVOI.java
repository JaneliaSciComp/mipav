package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import gov.nih.mipav.model.algorithms.AlgorithmTPSpline;
import gov.nih.mipav.model.algorithms.AlgorithmTransformVOI;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;

public class JDialogTransformVOI extends JDialogBase {

	/**
	 * 
	 */
	private static final long serialVersionUID = -8451501816656567049L;

	private ModelImage srcImage;
	
	private TransMatrix xfrm;
	
	private JRadioButton fileMatrix;
	
	private JRadioButton userDefinedMatrix;
	
	private JTextField matrixFName;
	
	private JTextField textTx, textTy, textTz, textRx, textRy, textRz;
	
	private JTextField textSx, textSy, textSz, textSKx, textSKy, textSKz;
	
	private float t_x, t_y, t_z;
	
	private JCheckBox allVOIBox;
	
	private JRadioButton cornerRB, centerRB, imCenterRB;
	
	public JDialogTransformVOI(ModelImage im){
		super(false);
		srcImage = im;
		init();
	}
	
	public void actionPerformed(ActionEvent e){
		String command = e.getActionCommand();
		if(command.equals("OK")){
			callAlgorithm();
		}else if(command.equals("Cancel")){
			dispose();
		}
	}
	
	protected void callAlgorithm(){
		boolean voiActive = false;
		VOIVector vois = srcImage.getVOIs();
		if(vois.isEmpty()){
			MipavUtil.displayError("There are no VOIs to transform");
			return;
		}
		for(VOI v : vois){
			VOIBaseVector vec = v.getCurves();
			for(VOIBase b : vec){
				if(b.isActive()){
					voiActive = true;
					break;
				}
			}
		}
		if(!(voiActive || allVOIBox.isSelected())){
			MipavUtil.displayError("Please select a VOI");
		}else if(setVariables()){
			srcImage.getParentFrame().getVOIManager().saveVOIs("TransformVOI");
			
			AlgorithmTransformVOI alg = new AlgorithmTransformVOI(srcImage, xfrm);
			alg.setTranslation(t_x, t_y, t_z);
			if(centerRB.isSelected()){
				alg.setCenter(AlgorithmTransformVOI.VOICENTER);
			} else if(cornerRB.isSelected()){
				alg.setCenter(AlgorithmTransformVOI.ORIGIN);
			} else if(imCenterRB.isSelected()){
				alg.setCenter(AlgorithmTransformVOI.IMCENTER);
			}
			
			alg.setAllVOIs(allVOIBox.isSelected());
			alg.run();
		}
	}
	
	private boolean setVariables(){
		String tmpStr;
		double Tx = 0, Ty = 0, Tz = 0, Rx, Ry, Rz, Sx, Sy, Sz, SKx, SKy, SKz;
		int msize = srcImage.getNDims() > 2 ? 4 : 3;
		xfrm = new TransMatrix(msize);
		
		if(userDefinedMatrix.isSelected()){
			tmpStr = textTx.getText();

            if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                Tx = Double.valueOf(tmpStr).doubleValue();
            } else {
                textTx.requestFocus();
                textTx.selectAll();

                return false;
            }

            tmpStr = textTy.getText();

            if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                Ty = Double.valueOf(tmpStr).doubleValue();
            } else {
                textTy.requestFocus();
                textTy.selectAll();

                return false;
            }

            tmpStr = textRz.getText();

            if (JDialogBase.testParameter(tmpStr, -360, 360)) {
                Rz = Double.valueOf(tmpStr).doubleValue();
            } else {
                textRz.requestFocus();
                textRz.selectAll();

                return false;
            }

            tmpStr = textSx.getText();

            if (JDialogBase.testParameter(tmpStr, 0, 300)) {
                Sx = Double.valueOf(tmpStr).doubleValue();
            } else {
                textSx.requestFocus();
                textSx.selectAll();

                return false;
            }

            tmpStr = textSy.getText();

            if (JDialogBase.testParameter(tmpStr, 0, 300)) {
                Sy = Double.valueOf(tmpStr).doubleValue();
            } else {
                textSy.requestFocus();
                textSy.selectAll();

                return false;
            }

            tmpStr = textSKx.getText();

            if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                SKx = Double.valueOf(tmpStr).doubleValue();
            } else {
                textSKx.requestFocus();
                textSKx.selectAll();

                return false;
            }

            tmpStr = textSKy.getText();

            if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                SKy = Double.valueOf(tmpStr).doubleValue();
            } else {
                textSKy.requestFocus();
                textSKy.selectAll();

                return false;
            }
            if ( (srcImage.getNDims() >= 3)) {
                tmpStr = textTz.getText();

                if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                    Tz = Double.valueOf(tmpStr).doubleValue();
                } else {
                    textTz.requestFocus();
                    textTz.selectAll();

                    return false;
                }

                tmpStr = textRx.getText();

                if (JDialogBase.testParameter(tmpStr, -360, 360)) {
                    Rx = Double.valueOf(tmpStr).doubleValue();
                } else {
                    textRx.requestFocus();
                    textRx.selectAll();

                    return false;
                }

                tmpStr = textRy.getText();

                if (JDialogBase.testParameter(tmpStr, -360, 360)) {
                    Ry = Double.valueOf(tmpStr).doubleValue();
                } else {
                    textRy.requestFocus();
                    textRy.selectAll();

                    return false;
                }

                tmpStr = textSz.getText();

                if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                    Sz = Double.valueOf(tmpStr).doubleValue();
                } else {
                    textSz.requestFocus();
                    textSz.selectAll();

                    return false;
                }

                tmpStr = textSKz.getText();

                if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                    SKz = Double.valueOf(tmpStr).doubleValue();
                } else {
                    textSKz.requestFocus();
                    textSKz.selectAll();

                    return false;
                }

                //xfrm.setTranslate(Tx, Ty, Tz);
                t_x = (float) Tx;
                t_y = (float) Ty;
                t_z = (float) Tz;
                xfrm.setRotate(Rx, Ry, Rz, TransMatrix.DEGREES);
                xfrm.setSkew(SKx, SKy, SKz);
                xfrm.setZoom(Sx, Sy, Sz);
            } else { // (image.getNDims() == 2) || (do25D)
                //xfrm.setTranslate(Tx, Ty);
            	t_x = (float) Tx;
                t_y = (float) Ty;
                t_z = 0;
                xfrm.setRotate(Rz);
                xfrm.setSkew(SKx, SKy);
                xfrm.setZoom(Sx, Sy);
            }
		}
		
		return true;
	}
	
	private void init(){
		
		final JPanel matrixPanel = new JPanel();
		matrixPanel.setBorder(buildTitledBorder("Transform"));
		matrixPanel.setLayout(new BoxLayout(matrixPanel, BoxLayout.Y_AXIS));
		matrixPanel.setForeground(Color.black);
		
		ButtonGroup matrixDeterminationGroup = new ButtonGroup();
		
		final JPanel filePanel = new JPanel(new BorderLayout());
        filePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        fileMatrix = new JRadioButton("Read matrix from file", false);
        fileMatrix.setFont(serif12);
        fileMatrix.setEnabled(false);
        fileMatrix.setAlignmentX(Component.LEFT_ALIGNMENT);
        matrixDeterminationGroup.add(fileMatrix);
        filePanel.add(fileMatrix, BorderLayout.WEST);
        fileMatrix.addItemListener(this);

        matrixFName = new JTextField(10);
        matrixFName.setFont(serif12);
        matrixFName.setAlignmentX(Component.LEFT_ALIGNMENT);
        matrixFName.setEnabled(false);
        filePanel.add(matrixFName);

        matrixPanel.add(filePanel);

        final String orientText = "<html>Image origin is in the upper left hand corner (first slice)." + "<P>"
                + "Righthand coordinate system.</html>";
        final JLabel orientIconLabel = new JLabel(orientText, MipavUtil.getIcon("orient.gif"), SwingConstants.LEFT);
        orientIconLabel.setFont(serif12);
        orientIconLabel.setForeground(Color.black);
        matrixPanel.add(orientIconLabel);

        userDefinedMatrix = new JRadioButton("User defined transformation matrix", false);
        userDefinedMatrix.setBounds(10, 95, 200, 25);
        userDefinedMatrix.setFont(serif12);
        userDefinedMatrix.setEnabled(true);
        userDefinedMatrix.setSelected(true);
        matrixDeterminationGroup.add(userDefinedMatrix);
        userDefinedMatrix.setAlignmentX(Component.LEFT_ALIGNMENT);
        matrixPanel.add(userDefinedMatrix);
        userDefinedMatrix.addItemListener(this);

        final JPanel translationPanel = new JPanel();
        translationPanel.setLayout(new GridBagLayout());
        translationPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelTx, labelTy, labelTz, labelRx, labelRy, labelRz;
        JLabel labelSx, labelSy, labelSz, labelSKx, labelSKy, labelSKz;
        
        // translation
        labelTx = new JLabel("Tx (mm)");
        labelTx.setForeground(Color.black);
        labelTx.setFont(serif12);

        textTx = new JTextField();
        textTx.setPreferredSize(new Dimension(45, 20));
        textTx.setMinimumSize(new Dimension(25, 20));
        textTx.setText("0");
        textTx.setFont(serif12);
        textTx.addFocusListener(this);

        labelTy = new JLabel("Ty");
        labelTy.setForeground(Color.black);
        labelTy.setBounds(40, 155, 50, 25);
        labelTy.setFont(serif12);

        textTy = new JTextField();
        textTy.setPreferredSize(new Dimension(45, 20));
        textTy.setMinimumSize(new Dimension(25, 20));
        textTy.setText("0");
        textTy.setFont(serif12);
        textTy.addFocusListener(this);

        labelTz = new JLabel("Tz");
        labelTz.setForeground(Color.black);
        labelTz.setFont(serif12);

        textTz = new JTextField();
        textTz.setPreferredSize(new Dimension(45, 20));
        textTz.setMinimumSize(new Dimension(25, 20));
        textTz.setText("0");
        textTz.setFont(serif12);
        textTz.addFocusListener(this);

        // rotation
        labelRx = new JLabel("Rx (degrees)");
        labelRx.setForeground(Color.black);
        labelRx.setFont(serif12);

        textRx = new JTextField();
        textRx.setPreferredSize(new Dimension(45, 20));
        textRx.setMinimumSize(new Dimension(25, 20));
        textRx.setText("0");
        textRx.setFont(serif12);
        textRx.addFocusListener(this);

        labelRy = new JLabel("Ry");
        labelRy.setForeground(Color.black);
        labelRy.setFont(serif12);

        textRy = new JTextField();
        textRy.setPreferredSize(new Dimension(45, 20));
        textRy.setMinimumSize(new Dimension(25, 20));
        textRy.setText("0");
        textRy.setFont(serif12);
        textRy.addFocusListener(this);

        labelRz = new JLabel("Rz");
        labelRz.setForeground(Color.black);
        labelRz.setFont(serif12);

        textRz = new JTextField();
        textRz.setPreferredSize(new Dimension(45, 20));
        textRz.setMinimumSize(new Dimension(25, 20));
        textRz.setText("0");
        textRz.setFont(serif12);
        textRz.addFocusListener(this);

        // scaling
        labelSx = new JLabel("Sx");
        labelSx.setForeground(Color.black);
        labelSx.setFont(serif12);

        textSx = new JTextField();
        textSx.setPreferredSize(new Dimension(45, 20));
        textSx.setMinimumSize(new Dimension(25, 20));
        textSx.setText("1");
        textSx.setFont(serif12);
        textSx.addFocusListener(this);

        labelSy = new JLabel("Sy");
        labelSy.setForeground(Color.black);
        labelSy.setFont(serif12);

        textSy = new JTextField();
        textSy.setPreferredSize(new Dimension(45, 20));
        textSy.setMinimumSize(new Dimension(25, 20));
        textSy.setText("1");
        textSy.setFont(serif12);
        textSy.addFocusListener(this);

        labelSz = new JLabel("Sz");
        labelSz.setForeground(Color.black);
        labelSz.setFont(serif12);

        textSz = new JTextField();
        textSz.setPreferredSize(new Dimension(45, 20));
        textSz.setMinimumSize(new Dimension(25, 20));
        textSz.setText("1");
        textSz.setFont(serif12);
        textSz.addFocusListener(this);

        // skewing
        labelSKx = new JLabel("SKx");
        labelSKx.setForeground(Color.black);
        labelSKx.setFont(serif12);

        textSKx = new JTextField();
        textSKx.setPreferredSize(new Dimension(45, 20));
        textSKx.setMinimumSize(new Dimension(25, 20));
        textSKx.setText("0");
        textSKx.setFont(serif12);
        textSKx.addFocusListener(this);

        labelSKy = new JLabel("SKy");
        labelSKy.setForeground(Color.black);
        labelSKy.setFont(serif12);

        textSKy = new JTextField();
        textSKy.setPreferredSize(new Dimension(45, 20));
        textSKy.setMinimumSize(new Dimension(25, 20));
        textSKy.setText("0");
        textSKy.setFont(serif12);
        textSKy.addFocusListener(this);

        labelSKz = new JLabel("SKz");
        labelSKz.setForeground(Color.black);
        labelSKz.setFont(serif12);

        textSKz = new JTextField();
        textSKz.setPreferredSize(new Dimension(45, 20));
        textSKz.setMinimumSize(new Dimension(25, 20));
        textSKz.setText("0");
        textSKz.setFont(serif12);
        textSKz.addFocusListener(this);

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(2, 2, 2, 2);

        gbc.gridx = 0;
        gbc.gridy = 0;
        translationPanel.add(Box.createHorizontalStrut(15), gbc);
        gbc.gridx = 1;
        translationPanel.add(labelTx, gbc);
        gbc.gridx = 2;
        translationPanel.add(textTx, gbc);
        gbc.gridx = 3;
        translationPanel.add(Box.createHorizontalStrut(5), gbc);
        gbc.gridx = 4;
        translationPanel.add(labelRx, gbc);
        gbc.gridx = 5;
        translationPanel.add(textRx, gbc);
        gbc.gridx = 6;
        translationPanel.add(Box.createHorizontalStrut(5), gbc);
        gbc.gridx = 7;
        translationPanel.add(labelSx, gbc);
        gbc.gridx = 8;
        translationPanel.add(textSx, gbc);
        gbc.gridx = 9;
        translationPanel.add(Box.createHorizontalStrut(5), gbc);
        gbc.gridx = 10;
        translationPanel.add(labelSKx, gbc);
        gbc.gridx = 11;
        translationPanel.add(textSKx, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        translationPanel.add(labelTy, gbc);
        gbc.gridx = 2;
        translationPanel.add(textTy, gbc);
        gbc.gridx = 4;
        translationPanel.add(labelRy, gbc);
        gbc.gridx = 5;
        translationPanel.add(textRy, gbc);
        gbc.gridx = 7;
        translationPanel.add(labelSy, gbc);
        gbc.gridx = 8;
        translationPanel.add(textSy, gbc);
        gbc.gridx = 10;
        translationPanel.add(labelSKy, gbc);
        gbc.gridx = 11;
        translationPanel.add(textSKy, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        translationPanel.add(labelTz, gbc);
        gbc.gridx = 2;
        translationPanel.add(textTz, gbc);
        gbc.gridx = 4;
        translationPanel.add(labelRz, gbc);
        gbc.gridx = 5;
        translationPanel.add(textRz, gbc);
        gbc.gridx = 7;
        translationPanel.add(labelSz, gbc);
        gbc.gridx = 8;
        translationPanel.add(textSz, gbc);
        gbc.gridx = 10;
        translationPanel.add(labelSKz, gbc);
        gbc.gridx = 11;
        translationPanel.add(textSKz, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 3;
        allVOIBox = new JCheckBox("Transform all VOIs");
        allVOIBox.setFont(serif12);
        translationPanel.add(allVOIBox, gbc);
        
        matrixPanel.add(translationPanel);
        
        textTx.setEnabled(true);
        textTy.setEnabled(true);
        textTz.setEnabled(true);
        textRx.setEnabled(true);
        textRy.setEnabled(true);
        textRz.setEnabled(true);
        textSx.setEnabled(true);
        textSy.setEnabled(true);
        textSz.setEnabled(true);
        textSKx.setEnabled(true);
        textSKy.setEnabled(true);
        textSKz.setEnabled(true);
        if(srcImage.is2DImage()){
        	textTz.setEnabled(false);
        	textRx.setEnabled(false);
            textRy.setEnabled(false);
            textSz.setEnabled(false);
            textSKz.setEnabled(false);
        }

        getContentPane().add(matrixPanel, BorderLayout.NORTH);
        
        JPanel centerPanel = new JPanel(new GridLayout());
        centerPanel.setForeground(Color.black);
        centerPanel.setBorder(buildTitledBorder("Rotate about: "));
        
        ButtonGroup locationGroup = new ButtonGroup();
        
        centerRB = new JRadioButton("VOI center");
        centerRB.setFont(serif12);
        centerRB.setSelected(true);
        locationGroup.add(centerRB);
        centerPanel.add(centerRB);
        
        cornerRB = new JRadioButton("Image origin");
        cornerRB.setFont(serif12);
        locationGroup.add(cornerRB);
        centerPanel.add(cornerRB);

        imCenterRB = new JRadioButton("Image center");
        imCenterRB.setFont(serif12);
        locationGroup.add(imCenterRB);
        centerPanel.add(imCenterRB);
        
        getContentPane().add(centerPanel, BorderLayout.CENTER);
        
        buildOKCancelButtons();
        
        JPanel buttonPanel = new JPanel();
        buttonPanel.setForeground(Color.black);
        buttonPanel.add(OKButton, BorderLayout.WEST);
        buttonPanel.add(cancelButton, BorderLayout.EAST);
        
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        
        pack();
        setVisible(true);
        System.gc();
        
	}
	
	//Difficulty in transforming the VOI from a file is that traslation will
	//already be a part of the matrix, where as we want it away from the matrix
	//No way to extract the translation portion of the transformation, which
	//might make using files a waste
	
	/**
     * Allows the user to select matrix file.
     * 
     * @return fileName
     */
    /*public String matrixFileMenu() {
        String fileName;
        JFileChooser chooser;
        fileName = null;

        // bring up file dialog
        try {
            chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MATRIX));

            final int returnVal = chooser.showOpenDialog(ViewUserInterface.getReference().getMainFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                matrixDirectory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                ViewUserInterface.getReference().setDefaultDirectory(matrixDirectory);
                matrixFName.setText(fileName);
                
                //TODO: Fill in user defined fields from here
                
            } else {
                return null;
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: JDialogScriptableTransform.displayMatrixFileMenu");

            return null;
        }

        readTransformMatrixFile(fileName);

        return fileName;
    }*/

    /**
     * Reads a matrix from a file.
     * 
     * @param fileName name of the matrix file.
     */
    /*public TransMatrix readTransformMatrixFile(final String fileName) {
    	
    	int nDims = image.getNDims() > 3 ? 4 : image.getNDims() + 1; 
    	
        TransMatrix matrix = new TransMatrix(nDims);
        matrix.identity();

        if (fileName == null) {
            MipavUtil.displayError("filename = null");
        }

        try {
            // search for file name relative to image first, then relative to MIPAV default, then absolute path
            File file = null;
            if (matrixDirectory != null) {
                file = new File(matrixDirectory + fileName);
            }
            if ((matrixDirectory == null) || (!file.exists())) {
                file = new File(image.getImageDirectory() + fileName);
            }
            if ( !file.exists()) {
                file = new File(ViewUserInterface.getReference().getDefaultDirectory() + fileName);
            }
            if ( !file.exists()) {
                file = new File(fileName);
            }

            final RandomAccessFile raFile = new RandomAccessFile(file, "r");
            
            final String extension = FileUtility.getExtension(file.getAbsolutePath()).toLowerCase();
            if (extension.equals(".tps")) {
                spline = new AlgorithmTPSpline(image);
                spline.readMatrix(raFile);
                raFile.close();
            } else if(extension.equals(".1d")) { 
            	fileTransMatrix = TransMatrix.readAfniMatrix(raFile);
            } else {
                spline = null;
                matrix.readMatrix(raFile, fileInterp, fileXres, fileYres, fileZres, fileXdim, fileYdim, fileZdim, 
                                  filetVOI, fileClip, filePad, false);
                raFile.close();
                fileTransMatrix = matrix;
            }

            // We don't know the coordinate system that the transformation represents. Therefore
            // bring up a dialog where the user can ID the coordinate system changes (i.e.
            // world coordinate and/or the "left-hand" coordinate system!
            // new JDialogOrientMatrix(parentFrame, (JDialogBase) this);
        } catch (final IOException error) {
            MipavUtil.displayError("Matrix read error");
            fileTransMatrix.identity();
        }
        
        return fileTransMatrix;
    }*/
	
}
