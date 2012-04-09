/*package PlugInT2;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;

public class PlugInDialogT2 extends JDialogBase implements AlgorithmInterface
{
	private ModelImage resultImage;
	private ModelImage image;
	private ViewUserInterface userInterface;

	public PlugInDialogT2(Frame inputFrame, ModelImage im)
	{
		super(inputFrame, true);
		if(im.getType() == ModelImage.BOOLEAN || im.isColorImage())
		{
			MipavUtil.displayError("Source Image must NOT be Boolean or Color");
			dispose();
			return;
		}

		image= im;
		userInterface= ((ViewJFrameBase)(inputFrame)).getUserInterface();
		init();
	}

	private void init()
	{
		setForeground(Color.BLACK);
		setTitle("T2 mapping");
		JPanel inputPanel = new JPanel(new GridLayout(3,3));
		inputPanel.setForeground(Color.BLACK);
		inputPanel.setBorder(buildTitledBorder("Input parameters:" ));

	}

	public void algorithmPerformed(AlgorithmBase arg0) 
	{
		// TODO Auto-generated method stub
	}

	public void actionPerformed(ActionEvent event) 
	{
		String command = event.getActionCommand();
		
		if(command.equals("OK"))
		{
			if(setVariables())
				callAlgorithm();
		}
		else if(command.equals("Script"))
			callAlgorithm();
		else if(command.equals("Cancel"))
			dispose();
	}

	private boolean setVariables()
	{
		String tmpStr;
		// TODO Auto-generated method stub
	}

	public ModelImage getResultImage()
	{
		return resultImage;
	}
}
*/