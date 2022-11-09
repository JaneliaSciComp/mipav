package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.view.*;

/**
*   Dialog to enter points for creating an Talaraich image.
*	@author William Gandler
*	@author Pierre-Louis Bazin
*	@see		TalairachTransformInfo
*	@see		AlgorithmTalairachTransform
*	@see		JDialogTalairachTransform
*	@see		JDialogACPC
*/
public class JDialogTLRC extends JDialogBase {
    
    private static final float MAX_ALLOWED_DEVIATION = 2.0f;
    private static final float MIN_ALLOWED_DEVIATION = 0.5f;
    
    private static final float ATLAS_FRONT_TO_AC = 70.0f;
    
    private static final float ATLAS_PC_TO_BACK  = 79.0f;

    private static final float ATLAS_BOT_TO_AC = 42.0f;
    private static final float ATLAS_AC_TO_TOP = 74.0f;
    private static final float ATLAS_AC_TO_LAT = 68.0f;

   
    
    private ViewJFrameTriImage   	frame;   
    private ModelImage           	originalImage;
	private ModelImage           	acpcImage;
	private ModelImage 				talairachImage;
	private	TalairachTransformInfo	transform;
   
	private     JButton                  applyTalairachButton;
	private     JButton                  cancelTalairachButton;
	private     JRadioButton             anteriorPt;
	private     JRadioButton             posteriorPt;
	private     JRadioButton             superiorPt;
	private     JRadioButton             inferiorPt;
	private     JRadioButton             leftPt;
	private     JRadioButton             rightPt;
	private     JButton                  setTalairachButton;
	private     JButton                  clearTalairachButton;
	private     Vector3f                 anteriorPt3Df;
	private     Vector3f                 posteriorPt3Df;
	private     Vector3f                 superiorPt3Df;
	private     Vector3f                 inferiorPt3Df;
	private     Vector3f                 leftPt3Df;
	private     Vector3f                 rightPt3Df;
	private     boolean                  haveAnteriorPt = false;
	private     boolean                  havePosteriorPt = false;
	private     boolean                  haveSuperiorPt = false;
	private     boolean                  haveInferiorPt = false;
	private     boolean                  haveLeftPt = false;
	private     boolean                  haveRightPt = false;
	private		int					 	 interpolation;
    
    
  	/**
  	*   This method creates a dialog for selecting markers used for
  	*   generating a Talairach view image from an AC-PC aligned view image
  	*   @param theParentFrame   The tri planar view frame that called this dialog.
  	*   @param orig              Image to generate a Talairach view from.
  	*   @param acpc              ACPC-aligned image
  	*   @param tlrc              Talairach-aligned image
  	*/
  	public JDialogTLRC(ViewJFrameTriImage theParentFrame, ModelImage orig, ModelImage acpc, ModelImage tlrc, TalairachTransformInfo trans, int interp) {
        super(theParentFrame, false);
        frame = theParentFrame;
        originalImage = orig;
		acpcImage = acpc;
		talairachImage = tlrc;
		transform = trans;
		interpolation = interp;
        init();
		
		// check for the transform info
		if (acpcImage.getTalairachTransformInfo()!=null) {
			TalairachTransformInfo transf = acpcImage.getTalairachTransformInfo();
			Vector3f pt = new Vector3f();
			if (transf.isTlrc()) {
				// check if the image is the AC-PC one
				if ( (acpcImage.getExtents()[0]==transf.getAcpcDim()[0]) && (acpcImage.getExtents()[1]==transf.getAcpcDim()[1]) && (acpcImage.getExtents()[2]==transf.getAcpcDim()[2]) ) {				
					Vector3f ac = transf.getAcpcAC();
					Vector3f pc = transf.getAcpcPC();
					Vector3f min = transf.getAcpcMin();
					Vector3f max = transf.getAcpcMax();
					
					pt.X = ac.X;
					pt.Y = min.Y;
					pt.Z = ac.Z;
					setAnteriorPt(pt);
					((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("A");
					((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(ViewJComponentTriImage.ANTERIOR_PT,pt);
					
					pt.X = ac.X;
					pt.Y = max.Y;
					pt.Z = ac.Z;
					setPosteriorPt(pt);
					((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("P");
					((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(ViewJComponentTriImage.POSTERIOR_PT,pt);
					
					pt.X = ac.X;
					pt.Y = ac.Y;
					pt.Z = min.Z;
					setInferiorPt(pt);
					((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("I");
					((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(ViewJComponentTriImage.INFERIOR_PT,pt);
					
					pt.X = pc.X;
					pt.Y = pc.Y;
					pt.Z = max.Z;
					setSuperiorPt(pt);
					((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("S");
					((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(ViewJComponentTriImage.SUPERIOR_PT,pt);
					
					pt.X = min.X;
					pt.Y = pc.Y;
					pt.Z = pc.Z;
					setRightPt(pt);
					((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("R");
					((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(ViewJComponentTriImage.RIGHT_PT,pt);
					
					pt.X = max.X;
					pt.Y = pc.Y;
					pt.Z = pc.Z;
					setLeftPt(pt);
					((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("L");
					((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(ViewJComponentTriImage.LEFT_PT,pt);
				}
			}
        }
		
		pack();
		setVisible(true);
    }
      
    /**
    *   Initializes GUI components of dialog.
    */
    private void init() {
  	    
  	    setTitle("Create Talairach Image");
  	    
  	    JPanel pointPanel = new JPanel(new GridLayout(6,1));
		pointPanel.setForeground(Color.black);
		pointPanel.setBorder(buildTitledBorder("Select point type"));
  	    
  	    ButtonGroup talairachGroup = new ButtonGroup();
        anteriorPt = new JRadioButton("Most anterior point", true);
		anteriorPt.setFont(serif12);
		anteriorPt.addActionListener(this);
		anteriorPt.setActionCommand("anteriorPtCommand");
		talairachGroup.add(anteriorPt);
		pointPanel.add(anteriorPt);		
		
		posteriorPt = new JRadioButton("Most posterior point",false);
		posteriorPt.setFont(serif12);
		posteriorPt.addActionListener(this);
		posteriorPt.setActionCommand("posteriorPtCommand");
		talairachGroup.add(posteriorPt);
		pointPanel.add(posteriorPt);
		
		superiorPt = new JRadioButton("Most superior point", false);
		superiorPt.setFont(serif12);
		superiorPt.addActionListener(this);
		superiorPt.setActionCommand("superiorPtCommand");
		talairachGroup.add(superiorPt);
		pointPanel.add(superiorPt);
		
		inferiorPt = new JRadioButton("Most inferior point", false);
		inferiorPt.setFont(serif12);
		inferiorPt.addActionListener(this);
		inferiorPt.setActionCommand("inferiorPtCommand");
		talairachGroup.add(inferiorPt);
		pointPanel.add(inferiorPt);
		
		leftPt = new JRadioButton("Most left point", false);
		leftPt.setFont(serif12);
		leftPt.addActionListener(this);
		leftPt.setActionCommand("leftPtCommand");
		talairachGroup.add(leftPt);
		pointPanel.add(leftPt);
		
		rightPt = new JRadioButton("Most right point", false);
		rightPt.setFont(serif12);
		rightPt.addActionListener(this);
		rightPt.setActionCommand("rightPtCommand");
		talairachGroup.add(rightPt);
		pointPanel.add(rightPt);
		
		setTalairachButton = new JButton("Set");
		setTalairachButton.setFont(serif12B);
		setTalairachButton.addActionListener(this);
		setTalairachButton.setActionCommand("setTalairach");
		//setTalairachButton.setPreferredSize(buttonSize);
	
		clearTalairachButton = new JButton("Clear");
		clearTalairachButton.setFont(serif12B);
		clearTalairachButton.addActionListener(this);
		clearTalairachButton.setActionCommand("clearTalairach");
		clearTalairachButton.setEnabled(false);
		//clearTalairachButton.setPreferredSize(buttonSize);
  	    
  	    applyTalairachButton  = new JButton("Apply");
        applyTalairachButton.setFont(serif12B);
        applyTalairachButton.addActionListener(this);
        applyTalairachButton.setActionCommand("applyTalairach");
        applyTalairachButton.setEnabled(false);
		//applyTalairachButton.setPreferredSize(buttonSize);
        
  	    cancelTalairachButton = new JButton("Cancel");
        cancelTalairachButton.setFont(serif12B);
        cancelTalairachButton.addActionListener(this);
        cancelTalairachButton.setActionCommand("cancelTalairach");
		//cancelTalairachButton.setPreferredSize(buttonSize);
		
		JPanel buttonPanel = new JPanel(new GridBagLayout());
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        buttonPanel.add(setTalairachButton, gbc);
		gbc.gridx = 1;
		buttonPanel.add(clearTalairachButton, gbc);
		gbc.gridx = 0; gbc.gridy = 1;
        buttonPanel.add(applyTalairachButton, gbc);
        gbc.gridx = 1;
        buttonPanel.add(cancelTalairachButton, gbc);
        buttonPanel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
        
        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
        mainPanel.add(pointPanel);
        
        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
  	}
  	
    /**
    *   If user clicks "Set", sets point here and in component image.  
    *   If user clicks "Clear", clears point here and in component image.
    *   If user clicks "Apply", creates new Talairach image based on points.
    *   If user clicks "Cancel", disposes this dialog.
    *   @param event    Event that triggered this method.
    */
	public void actionPerformed(ActionEvent event) {
	    String command = event.getActionCommand();
        int pointType;
	    Vector3f pt;
	    boolean found;
     
      if (command.equals("setTalairach")) {
            pt = new Vector3f(frame.getSagittalComponentSlice(), frame.getCoronalComponentSlice(), frame.getAxialComponentSlice());
            //System.out.println("pt: " + (int)pt.X + "," + (int)pt.Y + "," + (int)pt.Z);
			//System.out.println("corrected pt: " + (int)toOriginal(pt).X + "," + (int)toOriginal(pt).Y + "," + (int)toOriginal(pt).Z);
			if (anteriorPt.isSelected()) {
                pointType = ViewJComponentTriImage.ANTERIOR_PT;
                ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("A");
                setAnteriorPt(toOriginal(pt));
				posteriorPt.setSelected(true);
				if (havePosteriorPt) {
					setTalairachButton.setEnabled(false);
					clearTalairachButton.setEnabled(true);
				}
				else {
					setTalairachButton.setEnabled(true);
					clearTalairachButton.setEnabled(false);
				}
            }
            else if (posteriorPt.isSelected()) {
                pointType = ViewJComponentTriImage.POSTERIOR_PT;
                ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("P");
                setPosteriorPt(toOriginal(pt));
				superiorPt.setSelected(true);
				if (haveSuperiorPt) {
					setTalairachButton.setEnabled(false);
					clearTalairachButton.setEnabled(true);
				}
				else {
					setTalairachButton.setEnabled(true);
					clearTalairachButton.setEnabled(false);
				}
            }
            else if (superiorPt.isSelected()) {
                pointType = ViewJComponentTriImage.SUPERIOR_PT;
                ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("S");
                setSuperiorPt(toOriginal(pt));
				inferiorPt.setSelected(true);
				if (haveInferiorPt) {
					setTalairachButton.setEnabled(false);
					clearTalairachButton.setEnabled(true);
				}
				else {
					setTalairachButton.setEnabled(true);
					clearTalairachButton.setEnabled(false);
				}
            }
            else if (inferiorPt.isSelected()) {
                pointType = ViewJComponentTriImage.INFERIOR_PT;
                ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("I");
                setInferiorPt(toOriginal(pt));
				leftPt.setSelected(true);
				if (haveLeftPt) {
					setTalairachButton.setEnabled(false);
					clearTalairachButton.setEnabled(true);
				}
				else {
					setTalairachButton.setEnabled(true);
					clearTalairachButton.setEnabled(false);
				}
            }
            else if (leftPt.isSelected()) {
                pointType = ViewJComponentTriImage.LEFT_PT;
                ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("L");
                setLeftPt(toOriginal(pt));
				rightPt.setSelected(true);
				if (haveRightPt) {
					setTalairachButton.setEnabled(false);
					clearTalairachButton.setEnabled(true);
				}
				else {
					setTalairachButton.setEnabled(true);
					clearTalairachButton.setEnabled(false);
				}
            }
            else {
                pointType = ViewJComponentTriImage.RIGHT_PT;
                ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("R");
                setRightPt(toOriginal(pt));
            }
            ((ViewJComponentTriImage)frame.getTriImage(0)).setReferenceXY(pointType,pt);
        }
        else if (command.equals("clearTalairach")) {
            if (anteriorPt.isSelected()) {
                found = ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("A");
                if (found) {
                    haveAnteriorPt = false;
                    anteriorPt.setText("Most anterior point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                }
                else {
                    MipavUtil.displayError("Error! Failed to remove most anterior point");
                }
            }
            else if (posteriorPt.isSelected()) {
                found = ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("P");
                if (found) {
                    havePosteriorPt = false;
                    posteriorPt.setText("Most posterior point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                }
                else {
                    MipavUtil.displayError("Error! Failed to remove most posterior point");
                }
            }
            else if (superiorPt.isSelected()) {
                found = ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("S");
                if (found) {
                    haveSuperiorPt = false;
                    superiorPt.setText("Most superior point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                }
                else {
                    MipavUtil.displayError("Error! Failed to remove most superior point");
                }
            }
            else if (inferiorPt.isSelected()) {
                found = ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("I");
                if (found) {
                    haveInferiorPt = false;
                    inferiorPt.setText("Most inferior point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                }
                else {
                    MipavUtil.displayError("Error! Failed to remove most inferior point");
                }
            }
            else if (leftPt.isSelected()) {
                found = ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("L");
                if (found) {
                    haveLeftPt = false;
                    leftPt.setText("Most left point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                }
                else {
                    MipavUtil.displayError("Error! Failed to remove most left point");
                }
            }
            else  {
                found = ((ViewJComponentTriImage)frame.getTriImage(0)).removeReference("R");
                if (found) {
                    haveRightPt = false;
                    rightPt.setText("Most right point");
                    clearTalairachButton.setEnabled(false);
                    setTalairachButton.setEnabled(true);
                    applyTalairachButton.setEnabled(false);
                }
                else {
                    MipavUtil.displayError("Error! Failed to remove most right point");
                }
            }
        }
        else if (command.equals("anteriorPtCommand")) {
            if (haveAnteriorPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            }
            else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        }
        else if (command.equals("posteriorPtCommand")) {
            if (havePosteriorPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            }
            else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        }
        else if (command.equals("superiorPtCommand")) {
            if (haveSuperiorPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            }
            else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        }
        else if (command.equals("inferiorPtCommand")) {
            if (haveInferiorPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            }
            else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        }
        else if (command.equals("leftPtCommand")) {
            if (haveLeftPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            }
            else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        }
        else if (command.equals("rightPtCommand")) {
            if (haveRightPt) {
                setTalairachButton.setEnabled(false);
                clearTalairachButton.setEnabled(true);
            }
            else {
                setTalairachButton.setEnabled(true);
                clearTalairachButton.setEnabled(false);
            }
        }
        else if (command.equals("applyTalairach")) {
			setVisible(false);
            convertToTalairach();
			dispose();
        }
        else if (command.equals("cancelTalairach")) {
			frame.setVisible(false);
            setVisible(false);
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }
    
    /**
    *   Sets anterior label based on the point.  Enables "Apply" if all points have been set.
    *   @param pt   Point that was set.
    */
    private void setAnteriorPt(Vector3f pt) {
        anteriorPt.setSelected(true);
        posteriorPt.setSelected(false);
        superiorPt.setSelected(false);
        inferiorPt.setSelected(false);
        leftPt.setSelected(false);
        rightPt.setSelected(false);
        anteriorPt3Df = new Vector3f(pt.X, pt.Y, pt.Z);
        haveAnteriorPt = true;
        anteriorPt.setText("Most anterior point " + (int)(pt.X+1) + "," + (int)(pt.Y+1) + "," + (int)(pt.Z+1));
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);
        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) &&
            (haveLeftPt) && (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }
    
    /**
    *   Sets posterior label based on the point.  Enables "Apply" if all points have been set.
    *   @param pt   Point that was set.
    */
    private void setPosteriorPt(Vector3f pt) {
        anteriorPt.setSelected(false);
        posteriorPt.setSelected(true);
        superiorPt.setSelected(false);
        inferiorPt.setSelected(false);
        leftPt.setSelected(false);
        rightPt.setSelected(false);
        posteriorPt3Df = new Vector3f(pt.X, pt.Y, pt.Z);
        havePosteriorPt = true;
        posteriorPt.setText("Most posterior point " + (int)(pt.X+1) + "," + (int)(pt.Y+1) + "," + (int)(pt.Z+1));
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);
        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) &&
            (haveLeftPt) && (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }
    
    /**
    *   Sets superior label based on the point.  Enables "Apply" if all points have been set.
    *   @param pt   Point that was set.
    */
    private void setSuperiorPt(Vector3f pt) {
        anteriorPt.setSelected(false);
        posteriorPt.setSelected(false);
        superiorPt.setSelected(true);
        inferiorPt.setSelected(false);
        leftPt.setSelected(false);
        rightPt.setSelected(false);
        superiorPt3Df = new Vector3f(pt.X, pt.Y, pt.Z);
        haveSuperiorPt = true;
        superiorPt.setText("Most superior point " + (int)(pt.X+1) + "," + (int)(pt.Y+1) + "," + (int)(pt.Z+1));
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);
        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) &&
            (haveLeftPt) && (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }
    
    /**
    *   Sets inferior label based on the point.  Enables "Apply" if all points have been set.
    *   @param pt   Point that was set.
    */
    private void setInferiorPt(Vector3f pt) {
        anteriorPt.setSelected(false);
        posteriorPt.setSelected(false);
        superiorPt.setSelected(false);
        inferiorPt.setSelected(true);
        leftPt.setSelected(false);
        rightPt.setSelected(false);
        inferiorPt3Df = new Vector3f(pt.X, pt.Y, pt.Z);
        haveInferiorPt = true;
        inferiorPt.setText("Most inferior point " + (int)(pt.X+1) + "," + (int)(pt.Y+1) + "," + (int)(pt.Z+1));
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);
        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) &&
            (haveLeftPt) && (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }
    
    /**
    *   Sets left label based on the point.  Enables "Apply" if all points have been set.
    *   @param pt   Point that was set.
    */
    private void setLeftPt(Vector3f pt) {
        anteriorPt.setSelected(false);
        posteriorPt.setSelected(false);
        superiorPt.setSelected(false);
        inferiorPt.setSelected(false);
        leftPt.setSelected(true);
        rightPt.setSelected(false);
        leftPt3Df = new Vector3f(pt.X, pt.Y, pt.Z);
        haveLeftPt = true;
        leftPt.setText("Most left point " + (int)(pt.X+1) + "," + (int)(pt.Y+1) + "," + (int)(pt.Z+1));
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);
        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) &&
            (haveLeftPt) && (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }
    
    /**
    *   Sets right label based on the point.  Enables "Apply" if all points have been set.
    *   @param pt   Point that was set.
    */
    private void setRightPt(Vector3f pt) {
        anteriorPt.setSelected(false);
        posteriorPt.setSelected(false);
        superiorPt.setSelected(false);
        inferiorPt.setSelected(false);
        leftPt.setSelected(false);
        rightPt.setSelected(true);
        rightPt3Df = new Vector3f(pt.X, pt.Y, pt.Z);
        haveRightPt = true;
        rightPt.setText("Most right point " + (int)(pt.X+1) + "," + (int)(pt.Y+1) + "," + (int)(pt.Z+1));
        setTalairachButton.setEnabled(false);
        clearTalairachButton.setEnabled(true);
        if ((haveAnteriorPt) && (havePosteriorPt) && (haveSuperiorPt) && (haveInferiorPt) &&
            (haveLeftPt) && (haveRightPt)) {
            applyTalairachButton.setEnabled(true);
        }
    }
    
    /**
    *   Creates Talairach image based on points that were set in component images.
    */
    private void convertToTalairach() {
		Vector3f ac = transform.getAcpcAC();
		Vector3f pc = transform.getAcpcPC();
		float acpcRes = transform.getAcpcRes();
		float dist;
		float scale_A,scale_M,scale_P,scale_S,scale_I,scale_L,scale_R;
        
        // Check anterior distance
		dist = (ac.Y - anteriorPt3Df.Y)*acpcRes;
        if ((dist/ATLAS_FRONT_TO_AC < MIN_ALLOWED_DEVIATION) ||
            (dist/ATLAS_FRONT_TO_AC > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Front to Anterior commissure distance outside allowed range dist = "
            + dist + " Standard ATLAS_FRONT_TO_AC = " + ATLAS_FRONT_TO_AC);
            return;
        }
		scale_A = ATLAS_FRONT_TO_AC / dist;
		
		dist = (pc.Y - ac.Y)*acpcRes;
        scale_M = ViewJFrameTriImage.ATLAS_AC_TO_PC / dist;
        
		// Check posterior distance
		dist = (posteriorPt3Df.Y - pc.Y)*acpcRes;
        if ((dist/ATLAS_PC_TO_BACK < MIN_ALLOWED_DEVIATION) ||
            (dist/ATLAS_PC_TO_BACK > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Posterior commissure to back distance outside allowed range dist = "
            + dist + " Standard ATLAS_PC_TO_BACK = " + ATLAS_PC_TO_BACK);
            return;
        }
		scale_P = ATLAS_PC_TO_BACK / dist;
        
        // Check inferior distance
		dist = (ac.Z - inferiorPt3Df.Z)*acpcRes;
        if ((dist/ATLAS_BOT_TO_AC < MIN_ALLOWED_DEVIATION) ||
            (dist/ATLAS_BOT_TO_AC > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Bottom to Anterior commissure distance outside allowed range dist = "
            + dist + " Standard ATLAS_BOT_TO_AC = " + ATLAS_BOT_TO_AC);
            return;
        }
		scale_I = ATLAS_BOT_TO_AC / dist;
        
        // Check superior distance
		dist = (superiorPt3Df.Z - ac.Z)*acpcRes;
        if ((dist/ATLAS_AC_TO_TOP < MIN_ALLOWED_DEVIATION) ||
            (dist/ATLAS_AC_TO_TOP > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Anterior commissure to top distance outside allowed range dist = "
            + dist + " Standard ATLAS_AC_TO_TOP = " + ATLAS_AC_TO_TOP);
            return;
        }
        scale_S = ATLAS_AC_TO_TOP / dist;
        
        // Check left distance
		dist = (leftPt3Df.X - ac.X)*acpcRes;
        if ((dist/ATLAS_AC_TO_LAT < MIN_ALLOWED_DEVIATION) ||
            (dist/ATLAS_AC_TO_LAT > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Anterior commissure to left distance outside allowed range dist = "
            + dist + " Standard ATLAS_AC_TO_LAT = " + ATLAS_AC_TO_LAT);
            return;
        }
        scale_L = ATLAS_AC_TO_LAT / dist;
		
        // Check right distance 
		dist = (ac.X - rightPt3Df.X)*acpcRes;
        if ((dist/ATLAS_AC_TO_LAT < MIN_ALLOWED_DEVIATION) ||
            (dist/ATLAS_AC_TO_LAT > MAX_ALLOWED_DEVIATION)) {
            MipavUtil.displayError("Anterior commissure to right distance outside allowed range dist = "
            + dist + "Standard ATLAS_AC_TO_LAT = " + ATLAS_AC_TO_LAT);
            return;
        }
		scale_R = ATLAS_AC_TO_LAT / dist;
        
		// set the transform data
		Vector3f min = new Vector3f(rightPt3Df.X, anteriorPt3Df.Y, inferiorPt3Df.Z);
		Vector3f max = new Vector3f(leftPt3Df.X, posteriorPt3Df.Y, superiorPt3Df.Z);
		
		transform.setAcpcMin(min);
		transform.setAcpcMax(max);
		float[] res = new float[7];
		res[0] = acpcRes/scale_R;
		res[1] = acpcRes/scale_L;
		res[2] = acpcRes/scale_A;
		res[3] = acpcRes/scale_M;
		res[4] = acpcRes/scale_P;
		res[5] = acpcRes/scale_I;
		res[6] = acpcRes/scale_S;
		transform.setTlrcRes(res);
		transform.isTlrc(true);
			
        talairachImage = new ModelImage(acpcImage.getType(), transform.getTlrcDim(), makeImageName(acpcImage.getImageName(), "_Tlrc"));
        AlgorithmTalairachTransform algo = new AlgorithmTalairachTransform(talairachImage, originalImage, transform, AlgorithmTalairachTransform.ORIG_TO_TLRC, interpolation, true, true);                
		
		createProgressBar(originalImage.getImageName(), algo);
            
		algo.run();

		talairachImage.calcMinMax();
        talairachImage.setImageOrientation(FileInfoBase.AXIAL);

		int[] tlrcOrient = new int[3];
        tlrcOrient[0] = FileInfoBase.ORI_R2L_TYPE;
        tlrcOrient[1] = FileInfoBase.ORI_A2P_TYPE;
        tlrcOrient[2] = FileInfoBase.ORI_I2S_TYPE;
		float[] imgRes = new float[3];
		imgRes[0] = transform.getAcpcRes();
		imgRes[1] = transform.getAcpcRes();
		imgRes[2] = transform.getAcpcRes();
		int[] units = new int[3];
		units[0] = Unit.MILLIMETERS.getLegacyNum();
		units[1] = Unit.MILLIMETERS.getLegacyNum();
		units[2] = Unit.MILLIMETERS.getLegacyNum();

		for (int i = 0; i < transform.getTlrcDim()[2]; i++) {
			talairachImage.getFileInfo(i).setMin(talairachImage.getMin());
            talairachImage.getFileInfo(i).setMax(talairachImage.getMax());
	        talairachImage.getFileInfo(i).setUnitsOfMeasure(units);
	        talairachImage.getFileInfo(i).setResolutions(imgRes);
	        talairachImage.getFileInfo(i).setAxisOrientation(tlrcOrient);
	        talairachImage.getFileInfo(i).setImageOrientation(FileInfoBase.AXIAL);
        }
		setTalairachHeader(talairachImage);
        setTalairachHeader(originalImage);
        setTalairachHeader(acpcImage);
            
		try { 
			new ViewJFrameImage(talairachImage, null, new Dimension(610,200));
		}
		catch (OutOfMemoryError error){
			MipavUtil.displayError("Out of memory: unable to open new frame");
		}
                                   
		frame.updateImages(true);
		frame.setVisible(false);
	}
	
	/** add the Talairach Transform to the image header */
	public void setTalairachHeader(ModelImage img) {
		img.setTalairachTransformInfo(transform);
		/*
		for (int z=0;z<img.getFileInfo().length;z++) {
			img.getFileInfo()[z].setTransformID(FileInfoBase.TRANSFORM_TALAIRACH_TOURNOUX);
		}
		*/
	}
    
	/** to convert frame coordinates into the original image ones */
	private Vector3f toOriginal(Vector3f in) {
		int[] orient = acpcImage.getFileInfo(0).getAxisOrientation();
        int xDim = acpcImage.getExtents()[0];
        int yDim = acpcImage.getExtents()[1];
        int zDim = acpcImage.getExtents()[2];
        Vector3f out = new Vector3f( 0.0f, 0.0f, 0.0f );

        switch ( orient[0] ) {
        case FileInfoBase.ORI_R2L_TYPE:
            out.X = in.X;
            break;

        case FileInfoBase.ORI_L2R_TYPE:
            out.X = xDim - 1 - in.X;
            break;

        case FileInfoBase.ORI_A2P_TYPE:
            out.X = in.Y;
            break;

        case FileInfoBase.ORI_P2A_TYPE:
            out.X = xDim - 1 - in.Y;
            break;

        case FileInfoBase.ORI_I2S_TYPE:
            out.X = in.Z;
            break;

        case FileInfoBase.ORI_S2I_TYPE:
            out.X = xDim - 1 - in.Z;
            break;
        }

        switch ( orient[1] ) {
        case FileInfoBase.ORI_R2L_TYPE:
            out.Y = in.X;
            break;

        case FileInfoBase.ORI_L2R_TYPE:
            out.Y = yDim - 1 - in.X;
            break;

        case FileInfoBase.ORI_A2P_TYPE:
            out.Y = in.Y;
            break;

        case FileInfoBase.ORI_P2A_TYPE:
            out.Y = yDim - 1 - in.Y;
            break;

        case FileInfoBase.ORI_I2S_TYPE:
            out.Y = in.Z;
            break;

        case FileInfoBase.ORI_S2I_TYPE:
            out.Y = yDim - 1 - in.Z;
            break;
        }

        switch ( orient[2] ) {
        case FileInfoBase.ORI_R2L_TYPE:
            out.Z = in.X;
            break;

        case FileInfoBase.ORI_L2R_TYPE:
            out.Z = zDim - 1 - in.X;
            break;

        case FileInfoBase.ORI_A2P_TYPE:
            out.Z = in.Y;
            break;

        case FileInfoBase.ORI_P2A_TYPE:
            out.Z = zDim - 1 - in.Y;
            break;

        case FileInfoBase.ORI_I2S_TYPE:
            out.Z = in.Z;
            break;

        case FileInfoBase.ORI_S2I_TYPE:
            out.Z = zDim - 1 - in.Z;
            break;
        }

        return out;
    }

    /**
    *   Transforms and resamples volume using trilinear interpolation
    *   @param imgBuffer    Image array.
    *   @param xfrm         Transformation matrix to be applied.
    *   @param ires         In resolution (same in all dimensions).
    *   @param iXdim        In X dimension.
    *   @param iYdim        In Y dimension.
    *   @param iZdim        In Z dimension.
    *   @param oXres        Out X resolution.
    *   @param oYres        Out Y resolution.
    *   @param oZres        Out Z resolution.
    *   @param oXdim        Out X dimension.
    *   @param oYdim        Out Y dimension.
    *   @param oZdim        Out Z dimension.
    *   @param oXlow        Out X low.
    *   @param oYlow        Out Y low.
    *   @param oZlow        Out Z low.
    *   @param oXhigh       Out X high.
    *   @param oYhigh       Out Y high.
    *   @param oZhigh       Out Z high.
    *   @param progressBar  Progress bar.
    *   @param image        Image.
    */
	@SuppressWarnings("unused")
    private void transformTalairachTrilinear(float imgBuffer[], double xfrm[][], float ires,
                                             int iXdim, int iYdim, int iZdim, 
                                             float oXres, float oYres, float oZres, int oXdim, int oYdim, int oZdim,
                                             int oXlow, int oYlow, int oZlow, int oXhigh, int oYhigh, int oZhigh, 
                                             ViewJProgressBar progressBar, ModelImage image) {
        int   i,j,k;
        int   X0pos, Y0pos, Z0pos;
        int   X1pos, Y1pos, Z1pos;
        float X,Y,Z;
        float x0,y0,z0;
        float x1,y1,z1;
        float value;
        int   sliceSize;
        float imm, jmm, kmm;
        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3, temp4, temp5, temp6, temp7;
        int roundX, roundY, roundZ;
        sliceSize   = iXdim * iYdim;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        boolean doTransform;
        
        
        T00 = (float)xfrm[0][0]; T01 = (float)xfrm[0][1]; T02 = (float)xfrm[0][2]; T03 = (float)xfrm[0][3];
        T10 = (float)xfrm[1][0]; T11 = (float)xfrm[1][1]; T12 = (float)xfrm[1][2]; T13 = (float)xfrm[1][3];
        T20 = (float)xfrm[2][0]; T21 = (float)xfrm[2][1]; T22 = (float)xfrm[2][2]; T23 = (float)xfrm[2][3];
        
         
        for (i=oXlow; i <= oXhigh; i++) {
            progressBar.updateValue((int)((float)(i-oXlow)/(oXhigh - oXlow) * 100+.5), false);
            imm = (float)i * oXres;
            i1 = imm * T00 + T03;
            i2 = imm * T10 + T13;
            i3 = imm * T20 + T23;
            for (j=oYlow; j <= oYhigh; j++) {
                jmm = (float)j * oYres;
                j1 = jmm * T01;
                j2 = jmm * T11;
                j3 = jmm * T21;
                temp1 = i3 + j3;
                temp2 = i2 + j2;
                temp3 = i1 + j1;
                for (k=oZlow; k <= oZhigh; k++) {
                    //transform i,j,k
                    doTransform = false;
                    value = 0.0f;
                    kmm = (float)k * oZres;
                    X = (temp3 + (kmm*T02))/ires ; 
                    roundX = (int)(X+0.5f);
                    if ((X>=0) && (roundX<iXdim)){
                        Y = (temp2 + (kmm*T12))/ires;                           
                        roundY = (int)(Y+0.5f);
                        if ((Y>=0) && (roundY<iYdim)){
                            Z = (temp1 + (kmm*T22))/ires; 
                            roundZ= (int)(Z+0.5f);
                            if ((Z>=0) && (roundZ<iZdim)){
                                if ((roundX==iXdim-1)||(roundY==iYdim-1)||(roundZ==iZdim-1)) {
                                    X0pos = roundX;
                                    Y0pos = roundY*iXdim;
                                    Z0pos = roundZ*sliceSize;
                                    value = imgBuffer[Z0pos+Y0pos+X0pos];
                                    doTransform = true;
                                }
                            else {
                                    //set intensity of i,j,k to new transformed coordinate if
                                    //x,y,z is w/in dimensions of image
                                    x0 = X-(int)X;
                                    y0 = Y-(int)Y;
                                    z0 = Z-(int)Z;
                                    x1 = 1-x0;
                                    y1 = 1-y0;
                                    z1 = 1-z0;
                                    X0pos = (int)X;
                                    Y0pos = (int)Y*iXdim;
                                    Z0pos = (int)Z*sliceSize;
                                    X1pos = X0pos + 1;
                                    Y1pos = Y0pos + iXdim;
                                    Z1pos = Z0pos + sliceSize;
                                    temp4 = y1*z1;
                                    temp5 = y0*z1;
                                    temp6 = y1*z0;
                                    temp7 = y0*z0;
                                    value = x1*temp4*imgBuffer[Z0pos+Y0pos+X0pos] +
                                            x0*temp4*imgBuffer[Z0pos+Y0pos+X1pos] +
                                            x1*temp5*imgBuffer[Z0pos+Y1pos+X0pos] +
                                            x0*temp5*imgBuffer[Z0pos+Y1pos+X1pos] +
                                            x1*temp6*imgBuffer[Z1pos+Y0pos+X0pos] +
                                            x0*temp6*imgBuffer[Z1pos+Y0pos+X1pos] +
                                            x1*temp7*imgBuffer[Z1pos+Y1pos+X0pos] +
                                            x0*temp7*imgBuffer[Z1pos+Y1pos+X1pos];
                                    doTransform = true;
                                                
                                }
                            }// end if Z in bounds                        
                        }//end if Y in bounds
                    }// end if X in bounds
                    if (doTransform) {
                        image.set(i,j,k, value);
                    }
                }//end for k
            }//end for j
        }//end for i        
   }
    /**
     *     Transforms and resamples volume using trilinear interpolation
     */
	@SuppressWarnings("unused")
    private void computeTrilinearImage(float img[], float xi, float yi, float zi, int nix, int niy, int niz, 
										float[] result, int xr, int yr, int zr, int nrx, int nry, int nrz) {
        int xa,ya,za,xb,yb,zb;
        float ax,bx,ay,by,az,bz;
        
       if ( (xi>0) && (xi<nix-1) && (yi>0) && (yi<niy-1) && (zi>0) && (zi<niz-1)) {
            xa = (int)Math.floor(xi);
            ya = (int)Math.floor(yi);
            za = (int)Math.floor(zi);
            xb = xa + 1;
            yb = ya + 1;
            zb = za + 1;
            ax = xi - xa;
            bx = xb - xi;
            ay = yi - ya;
            by = yb - yi;
            az = zi - za;
            bz = zb - zi;

            result[ xr + nrx*yr + nrx*nry*zr ] =  bx*by*bz*img[ xa + nix*ya + nix*niy*za ]
                                                + ax*by*bz*img[ xb + nix*ya + nix*niy*za ]
                                                + bx*ay*bz*img[ xa + nix*yb + nix*niy*za ]
                                                + bx*by*az*img[ xa + nix*ya + nix*niy*zb ]
                                                + ax*ay*bz*img[ xb + nix*yb + nix*niy*za ]
                                                + bx*ay*az*img[ xa + nix*yb + nix*niy*zb ]
                                                + ax*by*az*img[ xb + nix*ya + nix*niy*zb ]
                                                + ax*ay*az*img[ xb + nix*yb + nix*niy*zb ];

        } else {
            result[ xr + nrx*yr + nrx*nry*zr ] = 0.0f;
        }
        return;
    }
}
