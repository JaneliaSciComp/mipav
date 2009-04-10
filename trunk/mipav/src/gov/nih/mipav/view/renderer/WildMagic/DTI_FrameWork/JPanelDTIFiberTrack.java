package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.*;

public class JPanelDTIFiberTrack extends JInterfaceBase {
	/**
     * 
     */
    private static final long serialVersionUID = -293668520247305166L;

    /** Box layout for control panel. */
	private Box contentBox;

	private JCheckBox reconstructTracts;

	private JButton computeButton;

    private VolumeTriPlanarInterfaceDTI parentFrame;
	
	public JPanelDTIFiberTrack(VolumeTriPlanarInterfaceDTI _parentFrame) {
		parentFrame = _parentFrame;
		mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());

		contentBox = new Box(BoxLayout.Y_AXIS);

		reconstructTracts = new JCheckBox("Tract Reconstruction");
		reconstructTracts.setSelected(true);
		contentBox.add(reconstructTracts);

		computeButton = new JButton("Compute Fibers");
		computeButton.setToolTipText("Compute fiber tracks");
		computeButton.addActionListener(this);
		computeButton.setActionCommand("ComputerFT");
		contentBox.add(computeButton);

		mainPanel.add(contentBox);

	}


	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		if (command.equalsIgnoreCase("ComputerFT")) {
			AlgorithmDTITract kTractAlgorithm = new AlgorithmDTITract(
					parentFrame.getDTIimage(), parentFrame.getEVimage(), parentFrame.getEValueimage(), 
					parentFrame.getParentDir() + "DTIImage.xml_tract", false, false, false);
			kTractAlgorithm.run();
			kTractAlgorithm.disposeLocal();
			kTractAlgorithm = null;
			parentFrame.setDTIParamsActive();
		}
	}
}