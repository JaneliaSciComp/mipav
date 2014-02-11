import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;








import java.awt.event.ActionEvent;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialog4DMaxProject extends JDialogBase implements
		AlgorithmInterface {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 4285561142126844278L;

	private ModelImage srcImage;
	
	private ModelImage dstImage;
	
	private PlugInAlgorithm4DMaxProject project;
	
	private JCheckBox saveBox;
	
	public PlugInDialog4DMaxProject(Frame theParentFrame, ModelImage image){
		super(theParentFrame, false);
		srcImage = image;
		int[] extents = image.getExtents();
		int[] newExtents = {extents[0], extents[1], extents[3]};
		dstImage = new ModelImage(image.getDataType(), newExtents, "Max Projections");
		
		init();
	}

	public void actionPerformed(ActionEvent e){
		String command = e.getActionCommand();
		
		if(command.equals("OK")){
			callAlgorithm();
		}
		else if(command.equals("Cancel")){
			dispose();
		}
	}
	
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub
		new ViewJFrameImage(dstImage);
		//new ViewJFrameImage(project.getLocationImage());
	}
	
	protected void callAlgorithm(){
		
		setVisible(false);
		project = new PlugInAlgorithm4DMaxProject(srcImage, dstImage, saveBox.isSelected());
		project.addListener(this);
		if (isRunInSeparateThread()) {
			if (project.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			project.run();
		}
		
	}
	
	private void init(){
		
		setTitle("4D Z-Max Project");
		
		JPanel checkPanel = new JPanel();
		checkPanel.setForeground(Color.black);
		
		saveBox = new JCheckBox("Save individual slices");
		saveBox.setFont(serif12);
		saveBox.setSelected(true);
		checkPanel.add(saveBox);
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.setForeground(Color.black);
		buildOKCancelButtons();
		buttonPanel.add(OKButton);
		buttonPanel.add(cancelButton);
		
		getContentPane().add(checkPanel, BorderLayout.CENTER);
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);
		
		pack();
        setVisible(true);
        setResizable(false);
        System.gc();
		
	}

}
