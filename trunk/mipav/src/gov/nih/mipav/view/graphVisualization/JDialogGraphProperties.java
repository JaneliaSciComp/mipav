package gov.nih.mipav.view.graphVisualization;


import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewMenuBar;
import gov.nih.mipav.view.ViewMenuBuilder;
import gov.nih.mipav.view.ViewUserInterface;
import hypergraph.applications.hexplorer.ContentHandlerFactory;
import hypergraph.graphApi.Graph;
import hypergraph.graphApi.GraphSystem;
import hypergraph.graphApi.GraphSystemFactory;
import hypergraph.graphApi.Node;
import hypergraph.graphApi.algorithms.GraphUtilities;
import hypergraph.graphApi.io.GraphWriter;
import hypergraph.graphApi.io.GraphXMLWriter;
import hypergraph.graphApi.io.SAXReader;
import hypergraph.visualnet.ArrowLineRenderer;
import hypergraph.visualnet.GraphPanel;
import hypergraph.graph.GraphImpl;
import hypergraph.graph.NodeImpl;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Enumeration;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.xml.sax.SAXException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJColorChooser;

public class JDialogGraphProperties extends JDialogBase {

	private JButton m_kBackgroundButton;
	private JButton m_kTextColorButton;
	private JButton m_kLineColorButton;
	private Color m_kBGBackup, m_kTextColorBackup, m_kLineColorBackup;
	private ViewJColorChooser colorChooser;
	private JDialogHyperGraph m_kParent;

	private JButton m_kTextBigger;
	private JButton m_kTextSmaller;
	private float[] m_afTextSize;

	public JDialogGraphProperties(JDialogHyperGraph parent) {
		super(parent, false);
		m_kParent = parent;
		init();
		setVisible(true);
	}

	public void init( ) {
		m_kBGBackup = new Color( m_kParent.getBackgroundColor().getRGB() );
		m_kTextColorBackup = new Color( m_kParent.getTextColor().getRGB() );
		m_kLineColorBackup = new Color( m_kParent.getLineColor().getRGB() );
		setForeground(Color.black);
		setTitle("Graph/Network Properties");

		final GridBagConstraints gbc = new GridBagConstraints();

		final JPanel mainPanel = new JPanel(new GridBagLayout());

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.insets = new Insets(15, 5, 5, 15);
		gbc.gridwidth = 1;

		m_afTextSize = new float[4];

		for ( int i = 0; i < 4; i++ )
		{
			m_afTextSize[i] = m_kParent.getTextSize()[i];
		}


		JLabel kLabel = new JLabel( "Change Text Size: " );
		m_kTextSmaller = new JButton( "-" );
		m_kTextSmaller.addActionListener(this);
		m_kTextSmaller.setActionCommand("DecreaseTextSize");
		m_kTextBigger = new JButton( "+" );
		m_kTextBigger.addActionListener(this);
		m_kTextBigger.setActionCommand("IncreaseTextSize");


		gbc.gridx = 0;
		mainPanel.add(kLabel, gbc);
		gbc.gridx = 1;
		mainPanel.add(m_kTextSmaller, gbc);
		gbc.gridx = 2;
		mainPanel.add(m_kTextBigger, gbc);
		gbc.gridy++;

		final JLabel backgroundLabel = new JLabel("Background Color");
		m_kBackgroundButton = new JButton();
		m_kBackgroundButton.setPreferredSize(new Dimension(25, 25));
		m_kBackgroundButton.setBackground( m_kParent.getBackgroundColor()  );
		m_kBackgroundButton.setToolTipText("Change background color");
		m_kBackgroundButton.addActionListener(this);
		m_kBackgroundButton.setActionCommand("backgroundColor");


		final JLabel textLabel = new JLabel("Text Color");
		m_kTextColorButton = new JButton();
		m_kTextColorButton.setPreferredSize(new Dimension(25, 25));
		m_kTextColorButton.setBackground( m_kParent.getTextColor() );
		m_kTextColorButton.setToolTipText("Change text color");
		m_kTextColorButton.addActionListener(this);
		m_kTextColorButton.setActionCommand("textColor");


		final JLabel lineLabel = new JLabel("Line Color");
		m_kLineColorButton = new JButton();
		m_kLineColorButton.setPreferredSize(new Dimension(25, 25));
		m_kLineColorButton.setBackground( m_kParent.getLineColor() );
		m_kLineColorButton.setToolTipText("Change line color");
		m_kLineColorButton.addActionListener(this);
		m_kLineColorButton.setActionCommand("lineColor");


		gbc.gridx = 0;
		mainPanel.add(backgroundLabel, gbc);
		gbc.gridx = 1;
		mainPanel.add(m_kBackgroundButton, gbc);

		gbc.gridx = 0;
		gbc.gridy++;
		//mainPanel.add(textLabel, gbc);
		gbc.gridx = 1;
		//mainPanel.add(m_kTextColorButton, gbc);

		gbc.gridx = 0;
		gbc.gridy++;
		//mainPanel.add(lineLabel, gbc);
		gbc.gridx = 1;
		//mainPanel.add(m_kLineColorButton, gbc);

		final JPanel OKCancelPanel = new JPanel();
		buildOKButton();
		OKButton.setActionCommand("OK");
		OKCancelPanel.add(OKButton, BorderLayout.WEST);
		buildCancelButton();
		cancelButton.setActionCommand("cancel");
		OKCancelPanel.add(cancelButton, BorderLayout.EAST);

		getContentPane().add(mainPanel, BorderLayout.CENTER);
		getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
		pack();
		setMinimumSize(getSize());
		setVisible(true);

	}

	public void actionPerformed(ActionEvent e) {
		final String command = e.getActionCommand();

		if (command.equalsIgnoreCase("backgroundColor")) {     
			colorChooser = new ViewJColorChooser(null, "Pick background color", new ActionListener()
			{ // OKAY listener
				public void actionPerformed(final ActionEvent ae) {
					m_kBackgroundButton.setBackground(colorChooser.getColor());
					m_kParent.setBackgroundColor( m_kBackgroundButton.getBackground() );
				}
			}, new ActionListener() { // CANCEL listener
				public void actionPerformed(final ActionEvent a) {}
			});
		}
		if (command.equalsIgnoreCase("textColor")) {     
			colorChooser = new ViewJColorChooser(null, "Pick text color", new ActionListener()
			{ // OKAY listener
				public void actionPerformed(final ActionEvent ae) {
					m_kTextColorButton.setBackground(colorChooser.getColor());
					m_kParent.setTextColor( m_kTextColorButton.getBackground() );
				}
			}, new ActionListener() { // CANCEL listener
				public void actionPerformed(final ActionEvent a) {}
			});
		}
		if (command.equalsIgnoreCase("lineColor")) {     
			colorChooser = new ViewJColorChooser(null, "Pick line color", new ActionListener()
			{ // OKAY listener
				public void actionPerformed(final ActionEvent ae) {
					m_kLineColorButton.setBackground(colorChooser.getColor());
					m_kParent.setLineColor( m_kLineColorButton.getBackground() );
				}
			}, new ActionListener() { // CANCEL listener
				public void actionPerformed(final ActionEvent a) {}
			});
		}
		if (command.equalsIgnoreCase("DecreaseTextSize")) {     
			m_kParent.increaseTextSize( false );
		}
		if (command.equalsIgnoreCase("IncreaseTextSize")) {     
			m_kParent.increaseTextSize( true );
		}
		if (command.equalsIgnoreCase("lineColor")) {     
			colorChooser = new ViewJColorChooser(null, "Pick line color", new ActionListener()
			{ // OKAY listener
				public void actionPerformed(final ActionEvent ae) {
					m_kLineColorButton.setBackground(colorChooser.getColor());
					m_kParent.setLineColor( m_kLineColorButton.getBackground() );
				}
			}, new ActionListener() { // CANCEL listener
				public void actionPerformed(final ActionEvent a) {}
			});
		}
		else if (command.equals("OK")) {
			m_kParent.setBackgroundColor( m_kBackgroundButton.getBackground() );
			m_kParent.savePreferences();
			setVisible(false);
		}
		else if (command.equals("cancel")) {
			m_kParent.setBackgroundColor( m_kBGBackup );
			m_kParent.setTextSize( m_afTextSize );
			dispose();
		}
	}


}
