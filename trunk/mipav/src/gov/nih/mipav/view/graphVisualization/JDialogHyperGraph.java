package gov.nih.mipav.view.graphVisualization;


import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageFileFilter;
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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;

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

public class JDialogHyperGraph extends JDialogBase{

	/** */
	private static final long serialVersionUID = 7133468293112430462L;

	JTextField m_kInputGraphTextField = null;

	/** current directory * */
	private String m_kCurrentDir = null;
	private String m_kFileName = null;
	
	private ModelImage m_kImage = null;

	public JDialogHyperGraph() {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		initInput();     
	}

	public JDialogHyperGraph(ViewJFrameImage kParent, ModelImage kImage) {
		super(kParent, false);
		initImage(kParent, kImage);     
		setSize( 900, 600 );
		setVisible(true);
	}
	
	public JDialogHyperGraph(String dir, String file) {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		init(dir, file);     
		setSize( 900, 600 );
		setVisible(true);
	}
	/** Stores the graph that the applet shows. */
	private GraphPanel graphPanel;

	public GraphPanel getGraphPanel() {
		return graphPanel;
	}
	/**@inheritDoc */
	public void init( String dir, String file ) {
		m_kCurrentDir = dir;
		m_kFileName = file;
		setTitle("Graph/Network Visualization");
		GraphSystem graphSystem = null;
		try {
			graphSystem = GraphSystemFactory.createGraphSystem("hypergraph.graph.GraphSystemImpl", null);
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(8);
		}
		Graph graph = null;
		URL url = null;
		URL codeBase = null;
		try {
			codeBase = new File(dir).toURI().toURL();
		} catch (MalformedURLException e1) {
			System.err.println( "codeBass null" );
			e1.printStackTrace();
		}
		try {
			url = new URL(codeBase, file);
			SAXReader reader = new SAXReader(graphSystem, url);
			ContentHandlerFactory ch = new ContentHandlerFactory();
			ch.setBaseUrl(codeBase);
			reader.setContentHandlerFactory(ch);
			graph = reader.parse();
		} catch (FileNotFoundException fnfe) {
			JOptionPane.showMessageDialog(null,
					"Could not find file " + url.getFile() + ". \n" +
					"Start applet with default graph", "File not found", JOptionPane.ERROR_MESSAGE);
			System.out.println("Exception : " + fnfe);
			fnfe.printStackTrace(System.out);
		} catch (SAXException saxe) {
			JOptionPane.showMessageDialog(null,
					"Error while parsing file" + url.getFile() + ". \n" +
					"Exception : " + saxe + ". \n" +
					"Start applet with default graph", "Parsing error", JOptionPane.ERROR_MESSAGE);
			System.out.println("Exception : " + saxe);
			saxe.getException().printStackTrace();
			saxe.printStackTrace(System.out);
		} catch (Exception e) {
			JOptionPane.showMessageDialog(null,
					"General error while reading file " + url + ". \n" +
					"Exception : " + e + ". \n" +
					"Start applet with default graph", "General error", JOptionPane.ERROR_MESSAGE);
			System.out.println(url);
			System.out.println("Exception : " + e);
			e.printStackTrace(System.out);
		}

		if (graph == null) {
			graph = GraphUtilities.createTree(graphSystem, 2, 3);
		}

		graphPanel = new GraphPanel(graph);
		file = new String("treelayout.prop");
		try {
			url = new URL(codeBase, file);
			graphPanel.loadProperties(url.openStream());
		} catch (FileNotFoundException fnfe) {
			JOptionPane.showMessageDialog(null,
					"Could not find propertyfile " + url.getFile() + ". \n" +
					"Start applet with default properties", "File not found", JOptionPane.ERROR_MESSAGE);
		} catch (Exception e) {
			JOptionPane.showMessageDialog(null,
					"General error while reading file " + url.getFile() + ". \n" +
					"Exception : " + e + ". \n" +
					"Start applet with default properties", "General error", JOptionPane.ERROR_MESSAGE);
			System.out.println(url);
			System.out.println("Exception : " + e);
			e.printStackTrace(System.out);
		}

		graphPanel.setLineRenderer(new ArrowLineRenderer());

		getContentPane().add(graphPanel);
	}

	public String getGraphXML() {
		try {
			OutputStream os = new ByteArrayOutputStream();
			GraphWriter graphWriter = new GraphXMLWriter(new OutputStreamWriter(os));
			graphWriter.write(getGraphPanel().getGraph());
			return os.toString();
		} catch (IOException ioe) {
			ioe.printStackTrace();
			return ioe.toString();
		}
	}

	private void initInput() {
		setForeground(Color.black);
		setTitle("Graph/Network Visualization");

		final GridBagConstraints gbc = new GridBagConstraints();

		final JPanel mainPanel = new JPanel(new GridBagLayout());

		final JLabel inputGraphLabel = new JLabel("Input Graph");

		m_kInputGraphTextField = new JTextField(20);
		m_kInputGraphTextField.setEditable(false);
		m_kInputGraphTextField.setBackground(Color.white);

		final JButton browseButton = new JButton("Browse");
		browseButton.addActionListener(this);
		browseButton.setActionCommand("Browse");

		final JButton outputDirBrowseButton = new JButton("Browse");
		outputDirBrowseButton.addActionListener(this);
		outputDirBrowseButton.setActionCommand("outputDirBrowse");

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.insets = new Insets(15, 5, 5, 15);
		gbc.gridwidth = 1;

		mainPanel.add(inputGraphLabel, gbc);

		gbc.gridx = 1;
		mainPanel.add(m_kInputGraphTextField, gbc);

		gbc.gridx = 2;
		mainPanel.add(browseButton, gbc);
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
		// setResizable(false);
		setVisible(true);

	}
	
	private void initImage(ViewJFrameImage kParent, ModelImage kImage)
	{
		m_kImage = kImage;
		JMenuBar kMenu = kParent.getJMenuBar();
		GraphSystem graphSystem = null;
		try {
			graphSystem = GraphSystemFactory.createGraphSystem("hypergraph.graph.GraphSystemImpl", null);
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(8);
		}
		Graph tree = graphSystem.createGraph();
		Node root = tree.createNode();
		root.setLabel( kImage.getImageName() );
		for ( int i = 0; i < kMenu.getMenuCount(); i++ )
		{
			Node menu = tree.createNode();
			menu.setLabel( kMenu.getMenu(i).getText() );
			tree.createEdge(root, menu );
			addSubTree( tree, menu, kMenu.getMenu(i).getMenuComponents() );
		}

		graphPanel = new MipavGraphPanel(tree, kParent);
		graphPanel.setLineRenderer(new ArrowLineRenderer());

		getContentPane().add(graphPanel);
	}
	

    public void addSubTree( Graph tree, Node menu, Component[] menuComponents )
    {
        if ( menuComponents == null )
        {
            return;
        }
        for ( int i = 0; i < menuComponents.length; i++ )
        {
            if ( menuComponents[i] instanceof JMenu )
            {
            	Node subMenu = tree.createNode();
            	subMenu.setLabel( ((JMenuItem)menuComponents[i]).getText() );
            	tree.createEdge( menu, subMenu );
    			addSubTree( tree, subMenu, ((JMenu)menuComponents[i]).getMenuComponents() );
            }
            else if ( menuComponents[i] instanceof JMenuItem )
            {
            	Node subMenu = tree.createNode();
            	subMenu.setLabel( ((JMenuItem)menuComponents[i]).getActionCommand() );
            	tree.createEdge( menu, subMenu );
            }
        }
    }

	@Override
	public void actionPerformed(ActionEvent e) {
		final String command = e.getActionCommand();

		if (command.equalsIgnoreCase("Browse")) {
			final JFileChooser chooser = new JFileChooser();

			if (m_kCurrentDir != null) {
				chooser.setCurrentDirectory(new File(m_kCurrentDir, m_kFileName));
			}
			chooser.setDialogTitle("Choose image");
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".xml", } ) );
			final int returnValue = chooser.showOpenDialog(this);
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				m_kCurrentDir = chooser.getSelectedFile().getAbsolutePath();
				final FileIO fileIO = new FileIO();
				fileIO.setQuiet(true);
				m_kFileName = chooser.getSelectedFile().getName();
				m_kInputGraphTextField.setText(m_kCurrentDir);
			}
		}
		else if (command.equals("OK")) {
			setVisible(false);
			new JDialogHyperGraph( m_kCurrentDir, m_kFileName);
		}
		else if (command.equals("cancel")) {
			dispose();
		}
	}
}
