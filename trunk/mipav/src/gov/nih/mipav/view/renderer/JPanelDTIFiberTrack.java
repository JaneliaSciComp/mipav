package gov.nih.mipav.view.renderer;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.surfaceview.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.*;
import javax.swing.event.*;

import javax.vecmath.*;

import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.*;

public class JPanelDTIFiberTrack extends JPanelRendererBase {
	/** Box layout for control panel. */
	private Box contentBox;

	private JCheckBox reconstructTracts;

	private JButton computeButton;

    private ViewJFrameVolumeViewDTI parentFrame;
	
	public JPanelDTIFiberTrack(ViewJFrameVolumeViewDTI _parentFrame) {
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
		Object source = event.getSource();
		String command = event.getActionCommand();
		if (command.equalsIgnoreCase("ComputerFT")) {
			AlgorithmDTITract kTractAlgorithm = new AlgorithmDTITract(
					parentFrame.getDTIimage(), parentFrame.getEVimage(), 
					parentFrame.getParentDir() + "DTIImage.xml_tract");
			kTractAlgorithm.run();
			kTractAlgorithm.disposeLocal();
			kTractAlgorithm = null;
			parentFrame.setDTIParamsActive();
		}
	}

	/**
	 * Get the main control Panel.
	 * 
	 * @return mainPanel main control panel
	 */
	public JPanel getMainPanel() {
		return mainPanel;
	}

}