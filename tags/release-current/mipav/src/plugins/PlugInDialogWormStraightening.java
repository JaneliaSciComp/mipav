import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.LineBorder;


public class PlugInDialogWormStraightening extends JDialogBase
{
	private static final long serialVersionUID = 2020338731116362598L;

	private ModelImage wormImage;
	
	private JTextField wormImageTextField, pointsFileTextField;
	
	//private String currDir;
	private String currDir = "C:\\images\\hari\\";	
	
	public PlugInDialogWormStraightening() {}
	
	public PlugInDialogWormStraightening(boolean modal)
	{
		init();
	}
	
	public void actionPerformed(ActionEvent e)
	{
		String command = e.getActionCommand();
		
		if (command.equalsIgnoreCase("wormImageBrowse")) {
            JFileChooser chooser = new JFileChooser();   
            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
            chooser.setDialogTitle("Choose image");
            if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                FileIO fileIO = new FileIO();
                if(wormImage != null) {
                	wormImage.disposeLocal();
                	wormImage = null;
                }
                wormImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory() + File.separator, false, null);
                //new ViewJFrameImage(wormImage);
                
                wormImageTextField.setText(currDir);
                //updateRes();
                
//                wormImage.getParentFrame().initResolutions();
//                wormImage.getParentFrame().updateImageExtents();
//                wormImage.getParentFrame().componentResized(null);
            }
        }else if(command.equals("pointsFileBrowse")) {
        	JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Choose points file");
            if (!currDir.equals("")) {
				chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".xml", ".voi" }));
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                String fileName = chooser.getSelectedFile().getName();
                String directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                currDir = chooser.getSelectedFile().getAbsolutePath();
                try {
                	FileVOI fileVOI = new FileVOI(fileName, directory, wormImage);
                	
                	VOI[] voi = fileVOI.readVOI(false);
                	
                	for (int i = 0; i < voi.length; i++) {
                		wormImage.registerVOI(voi[i]);
                    }

                	wormImage.notifyImageDisplayListeners();
                	
                	pointsFileTextField.setText(currDir);
                	
                	
                }catch(IOException ex) {
                	return;
                }
            }
        } else if(command.equals("ok")) {
        	setVisible(false);
        	callAlgorithm();
        	disposeLocal();
        } else if(command.equals("cancel")) {
        	setVisible(false);
        	disposeLocal();        	
        } else {
            super.actionPerformed(e);
        }

	}
		
	public void disposeLocal()
	{
		wormImage = null;
	}
	
	
	public void init()
	{
		setTitle("Worm Straightening");

		GridBagConstraints gbc = new GridBagConstraints();
		
		
		JPanel mainPanel = new JPanel(new GridBagLayout());
		
		
		JLabel wormImageLabel = new JLabel("Worm Image:");
		
		wormImageTextField = new JTextField(55);
		wormImageTextField.setEditable(false);
		wormImageTextField.setBackground(Color.white);
		wormImageTextField.setBorder(new LineBorder(Color.black));
		
		JButton wormImageBrowseButton = new JButton("Browse");
		wormImageBrowseButton.addActionListener(this);
		wormImageBrowseButton.setActionCommand("wormImageBrowse");
		
		JLabel pointsFileLabel = new JLabel("Points File:");
		
		pointsFileTextField = new JTextField(55);
		pointsFileTextField.setEditable(false);
		pointsFileTextField.setBackground(Color.white);
		pointsFileTextField.setBorder(new LineBorder(Color.black));
		
		JButton pointsFileBrowseButton = new JButton("Browse");
		pointsFileBrowseButton.addActionListener(this);
		pointsFileBrowseButton.setActionCommand("pointsFileBrowse");		
		
		gbc.anchor = GridBagConstraints.WEST;
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(wormImageLabel, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(wormImageTextField, gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(wormImageBrowseButton, gbc);
		
		
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(pointsFileLabel, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(pointsFileTextField, gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 1;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(pointsFileBrowseButton, gbc);
				
		JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton);
		
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
              
        
        pack();
        setResizable(false);
        setVisible(true);		
	}
		
	
	protected void callAlgorithm()
	{
		PlugInAlgorithmWormStraightening alg = new PlugInAlgorithmWormStraighteningAutomatic(wormImage);

		if (isRunInSeparateThread()) {

			// Start the thread as a low priority because we wish to still
			// have user interface work fast.
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
	}

}
