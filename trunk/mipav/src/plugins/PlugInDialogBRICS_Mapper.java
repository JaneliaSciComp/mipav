import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.view.*;

import gov.nih.tbi.commons.model.*;
import gov.nih.tbi.dictionary.model.DictionaryRestServiceModel.DataStructureList;
import gov.nih.tbi.dictionary.model.hibernate.*;
import gov.nih.tbi.repository.model.SubmissionType;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.*;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.transport.http.HTTPConduit;

public class PlugInDialogBRICS_Mapper extends JFrame implements ActionListener, ChangeListener, TreeSelectionListener, MouseListener,
        WindowListener, FocusListener {
    private static final long serialVersionUID = -5516621806537554154L;

    private final Font serif12  = new Font("Serif", Font.PLAIN, 12);
    private final Font serif12B = new Font("Serif", Font.BOLD, 12);

    private ScrollTextArea logOutputArea;

    private JScrollPane listPane;

    private JTable deTable;
    private LocalMappingTableModel deTableModel; 
    private FormStructure formStructure;
    private List<FormStructure> formStructureList = null;


    private JButton getStructsButton, selectStructButton, loadCSVButton, finishButton, saveMapButton, editDataElementsButton, outputDirButton;

    private JTextField outputDirTextField;
    private String outputDirBase;
    private JTabbedPane tabbedPane;
    
    private String csvFileDir;
    private File csvFile;

    private static final String STRUCT_GUID_SEPERATOR 	= "_-_";
    private static final String CSV_OUTPUT_DELIM 		= ",";
    private static final String BROWSE_NONIMG_DELIM 	= ";;;;";


    /** Dev data dictionary server. */
    @SuppressWarnings("unused")
    private static final String ddDevServer = "http://fitbir-dd-dev.cit.nih.gov/";

    /** Dev portal auth server. */
    @SuppressWarnings("unused")
    private static final String authDevServer = "http://fitbir-portal-dev.cit.nih.gov/";

    /** Stage data dictionary server. */
    @SuppressWarnings("unused")
    private static final String ddStageServer = "http://fitbir-dd-stage.cit.nih.gov/";

    /** Stage portal auth server. */
    @SuppressWarnings("unused")
    private static final String authStageServer = "http://fitbir-portal-stage.cit.nih.gov/";

    /** Demo data dictionary server. */
    @SuppressWarnings("unused")
    private static final String ddDemoServer = "http://fitbir-dd-demo.cit.nih.gov/";

    /** Demo portal auth server. */
    @SuppressWarnings("unused")
    private static final String authDemoServer = "http://fitbir-portal-demo.cit.nih.gov/";

    /** Prod data dictionary server. */
    private static final String ddProdServer = "https://dictionary.fitbir.nih.gov/";

    /** Prod portal auth server. */
    private static final String authProdServer = "https://fitbir.nih.gov/";

    /** File name of server configuration. */
    private static final String configFileName = "brics_config.properties";

    /** Property for reading the dd environment name from the BRICS config file. */
    private static final String ddEnvNameProp = "ddEnvName";

    /** Property for reading the dd server url from the BRICS config file. */
    private static final String ddServerURLProp = "ddServerURL";

    /** Property for reading the auth server url from the BRICS config file. */
    private static final String authServerURLProp = "authServerURL";

    /** Property for reading the dd authentication user name from the BRICS config file. */
    private static final String ddAuthUserProp = "ddAuthUser";

    /** Property for reading the dd authentication password from the BRICS config file. */
    private static final String ddAuthPassProp = "ddAuthPass";

    /**
     * Property for reading whether to use the (slower) authenticated web service, which allows testing against draft
     * forms.
     */
    private static final String ddUseAuthServiceProp = "ddUseAuthService";

    /** DD server environment name (Prod, Demo, Stage, or Dev). */
    private static String ddEnvName = "Prod";

    /** Full data dictionary server url */
    private static String ddServerURL = ddProdServer;

    /** Full authentication server url */
    private static String authServerURL = authProdServer;
    private static String ddAuthUser = "";
    private static String ddAuthPass = "";
    private static boolean ddUseAuthService = false;   

    private final ArrayList<String> tempDirs = new ArrayList<String>();

    //private boolean isFinished = false;

    private static final String svnVersion 		= "$Rev: 15178 $";
    private static final String svnLastUpdate 	= "$Date: 2017-10-10 14:17:11 -0400 (Tue, 10 Oct 2017) $";
    //private static final String pluginVersion = MipavUtil.getSVNChangedDate(svnLastUpdate);
    private static final String pluginVersion 	= "Beta version 0.1";

    private javax.swing.SwingWorker<Object, Object> fileWriterWorkerThread;

    /**
     * Text of the privacy notice displayed to the user before the plugin can be used.
     */
    public static final String PRIVACY_NOTICE = "BRICS is a collaborative environment with privacy rules that pertain to the collection\n"
            + "and display of imaging data. Before accessing and using BRICS, please ensure that you\n"
            + "familiarize yourself with our privacy rules, available through the BRICS Rules of Behavior\n" + "document and supporting documentation.\n"
            + "\n" + "Collection of this information is authorized under 42 U.S.C. 241, 242, 248, 281(a)(b)(1)(P)\n"
            + "and 44 U.S.C. 3101. The primary use of this information is to facilitate medical research.\n"
            + "This information may be disclosed to researchers for research purposes, and to system \n"
            + "administrators for evaluation and data normalization.\n" + "\n"
            + "Rules governing submission of this information are based on the data sharing rules defined in\n"
            + "the Notice of Grant Award (NOGA). If you do not have a grant defining data sharing requirements,\n"
            + "data submission is voluntary. Data entered into BRICS will be used solely for scientific and\n"
            + "research purposes. Significant system update information may be posted on\n" + "the BRICS site as required.";

    private static final Comparator dataElementCompare = new Comparator<DataElementValue>() {
        @Override
        public int compare(final DataElementValue o1, final DataElementValue o2) {
            return new Integer(o1.getPosition()).compareTo(new Integer(o2.getPosition()));
        }
    };

    private static final Comparator mapElementCompare = new Comparator<MapElement>() {
        @Override
        public int compare(final MapElement o1, final MapElement o2) {
            return new Integer(o1.getPosition()).compareTo(new Integer(o2.getPosition()));
        }
    };

    private static final Comparator groupCompare = new Comparator<RepeatableGroup>() {
        @Override
        public int compare(final RepeatableGroup o1, final RepeatableGroup o2) {
            return new Integer(o1.getPosition()).compareTo(new Integer(o2.getPosition()));
        }
    };

    private static final Comparator valueRangeCompare = new Comparator<ValueRange>() {
        @Override
        public int compare(final ValueRange o1, final ValueRange o2) {
            return o1.compareTo(o2);
        }
    };
    
    
    /** Mapper constructor  */
    public PlugInDialogBRICS_Mapper () {
        super();

        	// MATT Need new dir for mapper
        outputDirBase = Preferences.getProperty(Preferences.PREF_BRICS_PLUGIN_OUTPUT_DIR);
        if (outputDirBase == null) {
            outputDirBase = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "BRICS_Mapping" + File.separator;
            Preferences.setProperty(Preferences.PREF_BRICS_PLUGIN_OUTPUT_DIR, outputDirBase);
        }
        
        // Matt Need new dir for source CSV
        csvFileDir = Preferences.getProperty(Preferences.PREF_BRICS_PLUGIN_CSV_DIR);
        if (csvFileDir == null) {
            csvFileDir = ViewUserInterface.getReference().getDefaultDirectory();
        }

        // try to read the server config from disk, if it is there.
        // otherwise the value set above at initialization is used.
        readConfig();

        init();
        setVisible(true);
        
        validate();
        
        final int response = JOptionPane.showConfirmDialog(this, PlugInDialogBRICS_Mapper .PRIVACY_NOTICE, "Data Mapping Tool",
                JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        if (response == JOptionPane.NO_OPTION) {
            if (ViewUserInterface.getReference() != null && ViewUserInterface.getReference().isPlugInFrameVisible()) {
                System.gc();
                System.exit(0);
            } else {
                return;
            }
        }
       
        
        // Gets full list of form structures from the BRICS data dictionary
        //final Thread thread = new FormListRESTThread(this);
        //thread.start();
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
    	
        final String command = e.getActionCommand();

        if (command.equalsIgnoreCase("GetStructs")) {

        	final Thread thread = new FormListRESTThread(this);
            thread.start();

        } else if (command.equalsIgnoreCase("SelectStruct")) {
        	new ChooseFormStructDialog(this);  
        	
    	} else if (command.equalsIgnoreCase("LoadCSV")) {

            final JFileChooser chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File(csvFileDir));
            chooser.setDialogTitle("Choose CSV file");
            chooser.setFileFilter(new FileNameExtensionFilter("Comma separated value files (.csv)", "csv"));
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                csvFile = chooser.getSelectedFile();
                readCSVFile();

                csvFileDir = chooser.getSelectedFile().getAbsolutePath() + File.separator;
                Preferences.setProperty(Preferences.PREF_BRICS_PLUGIN_CSV_DIR, csvFileDir);
            }
        } else if (command.equalsIgnoreCase("SaveMapFile")) {
        	
            
        } else if (command.equalsIgnoreCase("HelpWeb")) {
            showCDE(null);

        } else if (command.equalsIgnoreCase("Finish")) {

            if (fileWriterWorkerThread != null && fileWriterWorkerThread.isDone()) {
                dispose();
                if (JDialogStandalonePlugin.isExitRequired()) {
                    System.gc();
                    System.exit(0);
                }
            } else {

                fileWriterWorkerThread = new javax.swing.SwingWorker<Object, Object>() {
                    @Override
                    public Object doInBackground() {
                        try {
                            //createSubmissionFiles();
                        } catch (final Throwable e) {
                            e.printStackTrace();
                        }

                        return null;
                    }

                    @Override
                    public void done() {
                        //if (isFinished) {
                            finishButton.setText("Close");
                            finishButton.setEnabled(true);
                        //}
                    }
                };
            }
        } else if (command.equalsIgnoreCase("EditDataElements")) {

            final String dsName = (String) deTableModel.getValueAt(deTable.getSelectedRow(), 0);
            new populateBRICSStructureTable(this, dsName);

        } else if (command.equalsIgnoreCase("OutputDirBrowse")) {
            final JFileChooser chooser = new JFileChooser(outputDirBase);

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setDialogTitle("Choose output directory for mapping result files");
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                outputDirTextField.setText(chooser.getSelectedFile().getAbsolutePath() + File.separator);

                outputDirBase = chooser.getSelectedFile().getAbsolutePath() + File.separator;
                Preferences.setProperty(Preferences.PREF_BRICS_PLUGIN_OUTPUT_DIR, outputDirBase);
            }
        } 
    }

    @Override
    public void windowActivated(final WindowEvent e) {}

    @Override
    public void windowClosed(final WindowEvent e) {}

    @Override
    public void windowClosing(final WindowEvent e) {
        if (tempDirs.size() > 0) {
            for (int i = 0; i < tempDirs.size(); i++) {
                final String dir = tempDirs.get(i);
                final File f = new File(dir);
                if (f.exists()) {
                    try {
                        FileUtils.deleteDirectory(f);
                    } catch (final IOException ioe) {
                        ioe.printStackTrace();
                    }
                }
            }
        }

        if (JDialogStandalonePlugin.isExitRequired()) {
            ViewUserInterface.getReference().windowClosing(e);
        }
    }

    @Override
    public void windowDeactivated(final WindowEvent e) {}

    @Override
    public void windowDeiconified(final WindowEvent e) {}

    @Override
    public void windowIconified(final WindowEvent e) {}

    @Override
    public void windowOpened(final WindowEvent e) {}

    

    /**
     * called after validation is done
     */
    public void complete(final FormStructureData fsData, final boolean isComplete) {
        String value = "true";
        
    }


    @Override
    public void focusGained(final FocusEvent e) {
    	//System.out.println("Focus Gained");
    }

    @Override
    public void focusLost(final FocusEvent e) {
    	//System.out.println("Focus lost");
    }

  
    

    /**
     * Sets values based on knob along slider.
     * 
     * @param e Event that triggered this function
     */
    @Override
    public void stateChanged(final ChangeEvent e) {
        final Object source = e.getSource();
      
    }

    
    /**  Setup Mapping tool layout */
    private void init() {
        setTitle("Data Mapping Tool - " + pluginVersion + " (" + ddEnvName + ")");
        
        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final Exception e) {
            // setIconImage() is not part of the Java 1.5 API - catch any
            // runtime error on those systems
        }

        final JMenuBar menuBar = new JMenuBar();
        final JMenu menu = new JMenu("Help");
        menuBar.add(menu);

        final JMenuItem helpMenuItem = new JMenuItem("Help web page");
        helpMenuItem.setActionCommand("HelpWeb");
        helpMenuItem.addActionListener(this);
        menu.add(helpMenuItem);

        final JMenuItem jvmMenuItem = new JMenuItem("JVM information");
        jvmMenuItem.setActionCommand("AboutJava");
        jvmMenuItem.addActionListener(ViewUserInterface.getReference());
        menu.add(jvmMenuItem);

        this.setJMenuBar(menuBar);
        
        final JPanel mapperConfig 	= new JPanel(new BorderLayout());
        final JPanel dataXForm 		= new JPanel(new BorderLayout());
        
        final JPanel fsDETablePanel = new JPanel(new GridBagLayout());
        
        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;
        gbc2.gridy = 0;
        gbc2.gridx = 1;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.fill = GridBagConstraints.BOTH;
        fsDETablePanel.add(buildStructurePanel(), gbc2);
    
        mapperConfig.add(buildButtonPanel(), BorderLayout.NORTH);
        mapperConfig.add(fsDETablePanel, BorderLayout.CENTER);
        mapperConfig.add(buildLogPanel(), BorderLayout.SOUTH);
        
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(serif12B);
        tabbedPane.addTab("Mapping Configuration", null, mapperConfig);
        tabbedPane.addTab("Data Transform", null, dataXForm);  // Nothing at the moment.

        getContentPane().add(tabbedPane, BorderLayout.CENTER);
        pack();
        validate();
        this.setMinimumSize(this.getSize());
        this.setResizable(true);
        centerOnScreen(this);
        
        addWindowListener(this);
    }

    
    private JPanel buildButtonPanel() {

        final JPanel buttonPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;

        
        getStructsButton = new JButton("Get Form Structures");
        getStructsButton.setToolTipText("Gets BRICS Form Structures from server. ");
        getStructsButton.addActionListener(this);
        getStructsButton.setActionCommand("GetStructs");
        
        selectStructButton = new JButton("Select Form Structure");
        selectStructButton.setToolTipText("Select BRICS Form Structure ");
        selectStructButton.addActionListener(this);
        selectStructButton.setActionCommand("SelectStruct");
        
        loadCSVButton = new JButton("Load Source DEs");
        loadCSVButton.setToolTipText("Load Source Data Elements CSV File");
        loadCSVButton.addActionListener(this);
        loadCSVButton.setActionCommand("LoadCSV");

        finishButton = new JButton("Generate Files");
        finishButton.setToolTipText("Generate Files");
        finishButton.addActionListener(this);
        finishButton.setActionCommand("Finish");

        editDataElementsButton = new JButton("Edit Data Elements");
        editDataElementsButton.setToolTipText("Edit data elements for selected Form Structure");
        editDataElementsButton.addActionListener(this);
        editDataElementsButton.setActionCommand("EditDataElements");

        //selectStructButton.setPreferredSize(defaultButtonSize);
        finishButton.setPreferredSize(defaultButtonSize);
        editDataElementsButton.setPreferredSize(defaultButtonSize);

        selectStructButton.setEnabled(false);
        loadCSVButton.setEnabled(false);
        editDataElementsButton.setEnabled(false);
        finishButton.setEnabled(false);

        gbc.gridx = 0;
        buttonPanel.add(getStructsButton, gbc);
        gbc.gridx++;
        buttonPanel.add(selectStructButton, gbc);
        gbc.gridx++;
        buttonPanel.add(loadCSVButton, gbc);
        gbc.gridx++;
        buttonPanel.add(editDataElementsButton, gbc);
        gbc.gridx++;
        buttonPanel.add(finishButton, gbc);

        return buttonPanel;
    }
    
    
    /**
     * 
     * @return JScrollPane of the table...
     */
    private JScrollPane buildStructurePanel() {
    	deTableModel = new LocalMappingTableModel();
    	deTableModel.addColumn("Group");
    	deTableModel.addColumn("Element Name");
    	deTableModel.addColumn("Title");
    	deTableModel.addColumn("Type");
    	deTableModel.addColumn("Reference PVs");
    	deTableModel.addColumn("Required");
    	deTableModel.addColumn("Source Name");
    	deTableModel.addColumn("Source Type");
    	deTableModel.addColumn("Source PVs");
    	deTableModel.addColumn("PV Mappings");

    
        
        deTable = new JTable(deTableModel) {
            private static final long serialVersionUID = 3053232611901005303L;

            @Override
            public String getToolTipText(final MouseEvent e) {
                String tooltip = "";

                final java.awt.Point p = e.getPoint();
                final int rowIndex = rowAtPoint(p);
                final int colIndex = columnAtPoint(p);
                ToolTipManager.sharedInstance().setDismissDelay(12000);
                FormStructureData fsInfo = new FormStructureData(formStructure);

                if(colIndex == deTableModel.getColumnIndex("Group")) {                      	
                        	
                	for (final RepeatableGroup group : formStructure.getRepeatableGroups() ) {    
                		
                		//If group name == table name at rowIndex, column index then add to tool tip.
                		if (group.getName().equals ((String) (deTable.getValueAt(rowIndex, colIndex)))) {      			
                			tooltip = "<html><p><b> Repeatability: </b>" + group.getType().getValue(); //+ "  <br/>";
                			tooltip += "  <b>:  </b>" + WordUtils.wrap(fsInfo.getStructInfo().getRepeatableGroupByName(group.getName()).getThreshold().toString(), 80, "<br/>", false) + "   <br/>";
                			tooltip += "</p></html>";
                		}
                	}
	                return tooltip;
                }
                else if(colIndex == deTableModel.getColumnIndex("Element Name") || colIndex == deTableModel.getColumnIndex("Title")) {
	                DataElement de = formStructure.getSortedDataElementList().get(rowIndex);
	                
	                tooltip = "<html><p><b> Name: </b> " + de.getName() + "<br/>";
	                tooltip += "<b> CDE Description: </b>" + WordUtils.wrap(de.getDescription(), 100, "<br/>", false) + "   <br/>";


	                //if (de.getNinds() != null && !de.getNinds().getValue().equals("")) {
	                //    tooltip += "<p><b> NINDS CDE ID: </b> " + de.getNinds().getValue() + "<br/>";
	                //}
	                //if (de.getGuidelines() != null && !de.getGuidelines().trim().equals("")) {
	                //    tooltip += "<p><b> Guidelines & Instructions: </b></br>"
	                 //           + WordUtils.wrap(removeRedundantDiseaseInfo(de.getGuidelines()), 100, "<br/>", false) + "</p>";
	                //}
	                //if (de.getNotes() != null && !de.getNotes().trim().equals("")) {
	                //    tooltip += "<p><b> Notes: </b><br/>" + WordUtils.wrap(removeRedundantDiseaseInfo(de.getNotes()), 100, "<br/>", false) + "</p>";
	                //}
	                tooltip += "</p></html>";
	                return tooltip;
                } 
                else if(colIndex == deTableModel.getColumnIndex("Reference PVs")) {
	                DataElement de = formStructure.getSortedDataElementList().get(rowIndex);
	                
	                tooltip += "<html> <p> <b> Type: </b> " + de.getType() + "   <br/>";
	                if (de.getMinimumValue() != null) {
	                	tooltip += "<b> Min: </b> " + de.getMinimumValue() + "<b>    Max:  </b>" + de.getMaximumValue() + "   <br/>";
	                }
	                if (de.getMeasuringUnit() != null) {
	                    tooltip += "<p><b> Unit of measure: </b> " + de.getMeasuringUnit().getDisplayName() + "</p>";
	                }
	                if (de.getValueRangeList().size() != 0) {
	                	tooltip += "<b> PV - PV Description: </b> " + WordUtils.wrap(de.getValueRangeList().toString().trim(), 80, "<br/>", false) + "<br/>";
	                }
	                tooltip += "</p></html>";
	                return tooltip;
                }
                else if(colIndex == deTableModel.getColumnIndex("Required")) {
                	tooltip += "<html> <p> <b> All REQUIRED elements must be mapped before saving mapping file. </b>";
                	tooltip += "</p></html>";
                	return tooltip;
                }
                else if(colIndex > deTableModel.getColumnIndex("Required") && colIndex < deTableModel.getColumnIndex("PV Mappings")) {
	                
	                tooltip += "<html> <p> <b> Single click: </b> " + " selects the DE to be mapped.  <br/>";
	                tooltip += " <b> Double click: </b> " + " for manual editting of the DE.  <br/>";	              
	                tooltip += "</p></html>";
	                return tooltip;
                }
                else if(colIndex == deTableModel.getColumnIndex("Source PVs")) {
	                
	                tooltip += "<html> <p> <b> Single click: </b> " + " selects the DE to be mapped.  <br/>";
	                tooltip += " <b> Double click: </b> " + " for manual editting of the DE.  <br/>";
	                tooltip += " <b>PVs can only contain a-z, A-Z and 0-9 separated by semicolons.</b>  <br/>";              
	                tooltip += "</p></html>";
	                return tooltip;
                }
                else if(colIndex == deTableModel.getColumnIndex("PV Mappings")) {
	                
	                tooltip += "<html> <p> <b> Single click: </b> " + " selects the DE to be mapped.  <br/>";
	                tooltip += " <b> Double click: </b> " + " for editting of the DE.  <br/>";              
	                tooltip += "</p></html>";
	                return tooltip;
                }
                
                else return null;
            }
        };
        
        deTable.addMouseListener(this);
        //deTable.addFocusListener(this);
        deTable.setPreferredScrollableViewportSize(new Dimension(1250, 300));
        deTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        deTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        deTable.getColumn("Group").setMinWidth(100);
        deTable.getColumn("Group").setMaxWidth(200);
        deTable.getColumn("Group").setPreferredWidth(125);
        //deTable.getColumn("Group").setCellRenderer(new CellRenderer());
        deTable.getColumn("Element Name").setMinWidth(100);
        deTable.getColumn("Element Name").setMaxWidth(200);
        deTable.getColumn("Element Name").setPreferredWidth(125);
        //deTable.getColumn("Element Name").setCellRenderer(new CellRenderer());
        
        deTable.getColumn("Title").setMinWidth(175);
        deTable.getColumn("Title").setMaxWidth(400);
        deTable.getColumn("Title").setPreferredWidth(225);
        
        deTable.getColumn("Required").setMinWidth(115);
        deTable.getColumn("Required").setMaxWidth(115);
        deTable.getColumn("Required").setPreferredWidth(115);
        deTable.getColumn("Required").setCellRenderer(new CellRenderer());
        
        deTable.getColumn("Source PVs").setCellRenderer(new CellRenderer());
        //deTable.getColumn("Source PVs").setCellEditor(cellEditor);
        //deTable.getColumn("Source PVs").set
        listPane = buildScrollPane(deTable);
        listPane.setBorder(buildTitledBorder(" Reference Form Structure:   "));
   
        return listPane;
    }
    
    /**
     * N
     * @author McAuliffe
     *
     */
    private class CellRenderer extends DefaultTableCellRenderer {

    	@Override
        public Component getTableCellRendererComponent(final JTable table, final Object value,
                final boolean isSelected, final boolean hasFocus, final int row, final int col) {
    		
            final Component cell = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col);
            
            TableCellRenderer refRenderer = table.getCellRenderer(row, col);
            if (refRenderer instanceof CellRenderer) { 
	
	            if (col == deTableModel.getColumnIndex("Required") && ((String) value).equalsIgnoreCase("REQUIRED")) { 
	                setForeground(Color.red);
	            } 
	            else {
	                setForeground(Color.black);
	            }
	            
	            if (col == 8 && hasFocus ) {
	            	//System.out.println(" Focus ==== " + hasFocus + " row = " + row);
	            	//System.out.println(" Selected ==== " + isSelected + " row = " + row);
	            } 
	            //else if (col == 8) {
	            	//System.out.println(" 2Focus ==== " + hasFocus + " row = " + row);
	            	//System.out.println(" 2Selected ==== " + isSelected + " row = " + row);
	                // validate format for row, col
	            	//if ( !deDef[2].matches("^[a-zA-Z0-9;]*$") ) {
               		// displayError("PVs can only contain a-z, A-Z and 0-9 separated by semicolons." + "  Row: " + row );
               		// fis.close();
               		// throw new Exception("Error reading file.");
                    //	}
	            //}
            }
            return cell;
    	}
    }
    
    
    /**
     * Build a panel log.
     */
    private JPanel buildLogPanel() {
        final JPanel destPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.CENTER;

        gbc2.gridy = 0;
        gbc2.gridx = 0;
        
        final JPanel outputDirPanel = new JPanel();
        final JLabel outputDirLabel = new JLabel("Output Directory for mapping result files: ");
        outputDirTextField = new JTextField(20);
        outputDirTextField.setEditable(false);
        outputDirTextField.setToolTipText(outputDirBase);
        outputDirTextField.setText(outputDirBase);
        outputDirButton = buildTextButton("Browse", "Choose Output Directory for mapping result files", "OutputDirBrowse", this);
        outputDirButton.setPreferredSize(defaultButtonSize);
        
        saveMapButton = new JButton("Save");
        saveMapButton.setToolTipText("Save Mapping File (Source DEs to BRICS DEs");
        saveMapButton.addActionListener(this);
        saveMapButton.setActionCommand("SaveMapFile");
        saveMapButton.setPreferredSize(defaultButtonSize);
        saveMapButton.setEnabled(false);

        outputDirPanel.add(outputDirLabel);
        outputDirPanel.add(outputDirTextField);
        outputDirPanel.add(outputDirButton);
        outputDirPanel.add(saveMapButton);
        destPanel.add(outputDirPanel, gbc2);

        logOutputArea = buildScrollTextArea(Color.white);
        logOutputArea.setBorder(buildTitledBorder("Output log"));
        logOutputArea.getTextArea().setEditable(false);
        logOutputArea.getTextArea().setRows(5);
        
        gbc2.gridy = 1;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.anchor = GridBagConstraints.NORTHWEST;
        destPanel.add(logOutputArea, gbc2);

        return destPanel;
    }

  
    /**
     * Pops up the
     * 
     */
    private boolean validateRequired() {
    	
    	int numRows = deTableModel.getRowCount();
    	for (int i=0; i< numRows; i++) {
    		if ( deTableModel.getValueAt(i, deTableModel.getColumnIndex("Required")).equals("REQUIRED") && 
    				(deTableModel.getValueAt(i, deTableModel.getColumnIndex("Source Name")) == null || 
    				((String)(deTableModel.getValueAt(i, deTableModel.getColumnIndex("Source Name")))).trim().equals(""))) {
    			saveMapButton.setEnabled(false);
    			return false;
    			
    		}
    	}
    	saveMapButton.setEnabled(true);
    	return true;
    }
   
    /**
     * writes out csv file
     * 
     * @param outputDirBase
     * @param outputFileNameBase
     * @param imageFile
     */
    private final String getCSVDataRow(final String outputDirBase, final String outputFileNameBase, final FormStructureData fsData, final int repeatNum) {
        String csvRow = new String();

        if (fsData == null) {
            return csvRow;
        }

        final ArrayList<RepeatableGroup> orderedGroupList = new ArrayList(fsData.getStructInfo().getRepeatableGroups());
        Collections.sort(orderedGroupList, groupCompare);

        for (final RepeatableGroup group : orderedGroupList) {
            if (fsData.isGroupRepeatSet(group.getName(), repeatNum)) {
                final GroupRepeat repeat = fsData.getGroupRepeat(group.getName(), repeatNum);
                final Vector<DataElementValue> deList = repeat.getDataElements();
                Collections.sort(deList, dataElementCompare);

                for (final DataElementValue deVal : deList) {
                    final String deName = deVal.getName();
                    String value = deVal.getValue();

                    // final File f = new File(value);
                    // if (f.isFile() || value.endsWith("_collision")) {
                    // if (value.endsWith("_collision")) {
                    // value = value.substring(0, value.indexOf("_collision"));
                    // final String filename = value.substring(value.lastIndexOf(File.separator) + 1, value.length());
                    // value = outputFileNameBase + File.separator + filename;
                    // } else {
                    // final String filename = f.getName();
                    // value = outputFileNameBase + File.separator + filename;
                    // }
                    // }

                    // escape commas in values - if there's a comma, put quotes
                    // around the value and double up any existing quotes
                    if (value.contains(CSV_OUTPUT_DELIM)) {
                        value = "\"" + value.replaceAll("\"", "\"\"") + "\"";
                    }

                    if (csvRow.length() == 0) {
                        csvRow = value;
                    } else {
                        csvRow += CSV_OUTPUT_DELIM + value;
                    }
                }
            } else {
                for (int i = 0; i < group.getSize(); i++) {
                    if (group.getPosition() != 0 || i != 0) {
                        csvRow += CSV_OUTPUT_DELIM;
                    }
                }
            }
        }

        return csvRow;
    }


    /**
     * Append a line to the log output area in the Log tab.
     * 
     * @param line The line to append (do not include the trailing newline).
     */
    private void printlnToLog(final String line) {
        logOutputArea.getTextArea().append(line + "\n");
    }

    
    @Override
    public void valueChanged(final TreeSelectionEvent e) {

    }

    @Override
    public void mouseClicked(final MouseEvent e) {
        final Component c = e.getComponent();
        
        if (c instanceof JTable) {
           /** if (deTable.getSelectedRow() == -1) {
                editDataElementsButton.setEnabled(false);
                saveMapButton.setEnabled(false);
                return;
            } else {
                if ( !isFinished) {
                    editDataElementsButton.setEnabled(true);
                    saveMapButton.setEnabled(true);
                }
            }*/


            if (e.getClickCount() == 2 && deTable.getSelectedColumn() < deTableModel.getColumnIndex("Source Name") ) {
                    showCDE(null);
            }
            else if (e.getClickCount() == 2 && deTable.getSelectedColumn() == deTableModel.getColumnIndex("Source PVs")) {
            	if (deTable.getSelectedRow() != -1 ){
	            	if ( deTable.getValueAt(deTable.getSelectedRow(), deTableModel.getColumnIndex("Source Name")) == null ||
	            	     deTable.getValueAt(deTable.getSelectedRow(), deTableModel.getColumnIndex("Source Name")).equals("") ) {
	            		displayWarning("Source Name cell is empty.");
	            		return;
	            	} else {
	            		new srcPVDialog (this);
	            	}
            	}
            }
            else if (e.getClickCount() == 2 && deTable.getSelectedColumn() == deTableModel.getColumnIndex("PV Mappings")) {
            	
            	// Check Source PVs are not empty
            	if (deTable.getSelectedRow() != -1 ){
	            	if ( deTable.getValueAt(deTable.getSelectedRow(), deTableModel.getColumnIndex("Source PVs")) == null ||
	            	     deTable.getValueAt(deTable.getSelectedRow(), deTableModel.getColumnIndex("Source PVs")).equals("") ) {
	            		displayWarning("Source PVs cell is empty.");
	            		return;
	            	} else {
	            		new PVMappingDialog (this);
	            	}
            	}
            }
        }
    }

    @Override
    public void mouseEntered(final MouseEvent e) {}

    @Override
    public void mouseExited(final MouseEvent e) {}

    @Override
    public void mousePressed(final MouseEvent e) {}

    @Override
    public void mouseReleased(final MouseEvent e) {}

    //public void ennableDisableFinishButton() {
        // boolean allCompleted = true;

        // for (int i = 0; i < sourceTableModel.getRowCount(); i++) {
        // final String completed = (String) sourceTableModel.getValueAt(i, 1);
        // if (completed.equalsIgnoreCase("No")) {
        // allCompleted = false;
        // break;
        // }
        // }

  //      if (deTableModel.getRowCount() == 0) {
  //          finishButton.setEnabled(false);
    //    } else {
            // changed to always enabled if some data added
  //          finishButton.setEnabled(true);
            // if (allCompleted) {
            // finishButton.setEnabled(true);
            // } else {
            // finishButton.setEnabled(false);
            // }
 //       }

   // }

   

    /**
     * Tries to read server configuration from BRICS config file on local disk.
     */
    private void readConfig() {
        final InputStream in = getClass().getResourceAsStream(configFileName);
        if (in != null) {
            final Properties prop = new Properties();
            try {
                prop.load(in);
            } catch (final IOException e) {
                //Preferences.debug("Unable to load BRICS preferences file: " + configFileName + "\n", Preferences.DEBUG_MINOR);
                e.printStackTrace();
            }
            // use pre-set, hardcoded values as defaults if properties are not found
            ddEnvName = prop.getProperty(ddEnvNameProp, ddEnvName);
            System.out.println("ddEnvName:\t" + ddEnvName);
            authServerURL = prop.getProperty(authServerURLProp, authServerURL);
            System.out.println("authServer:\t" + authServerURL);
            ddServerURL = prop.getProperty(ddServerURLProp, ddServerURL);
            System.out.println("ddServer:\t" + ddServerURL);
            ddAuthUser = prop.getProperty(ddAuthUserProp, ddAuthUser);
            System.out.println("ddAuthUser:\t" + ddAuthUser);
            ddAuthPass = prop.getProperty(ddAuthPassProp, ddAuthPass);
            System.out.println("ddAuthPass:\t" + ddAuthPass);
            ddUseAuthService = Boolean.parseBoolean(prop.getProperty(ddUseAuthServiceProp, "" + ddUseAuthService));
            System.out.println("ddUseAuthService:\t" + ddUseAuthService);
        }
    }


    private static final String removeRedundantDiseaseInfo(final String field) {
        final ArrayList<String> diseaseStrings = new ArrayList<String>();
        final ArrayList<String> fieldStrings = new ArrayList<String>();

        final Matcher m = Pattern.compile("(\\w[\\w\\s\\/()]*):?\\s+(.+)\\s+-----?\\s+").matcher(field);

        // get the first info
        if (m.find()) {
            diseaseStrings.add(m.group(1));
            fieldStrings.add(m.group(2));

            int lastMatchIndex = m.end();
            while (m.find()) {
                boolean foundFieldMatch = false;
                for (int i = 0; i < diseaseStrings.size() && !foundFieldMatch; i++) {
                    if (fieldStrings.get(i).equals(m.group(2))) {
                        // only add if the disease isn't a duplicate
                        boolean foundDisease = false;
                        final String[] split = diseaseStrings.get(i).split("/");
                        for (final String s : split) {
                            if (s.equals(m.group(1))) {
                                foundDisease = true;
                            }
                        }
                        if ( !foundDisease) {
                            diseaseStrings.set(i, diseaseStrings.get(i) + "/" + m.group(1));
                        }
                        foundFieldMatch = true;
                    }
                }

                // didn't find a matching field value, add a new one
                if ( !foundFieldMatch) {
                    diseaseStrings.add(m.group(1));
                    fieldStrings.add(m.group(2));
                }

                lastMatchIndex = m.end();
            }

            // many fields don't have ----- at the end of their last value, or are cut off by the character limit, so do
            // one last field value
            // this might still have problems if the text cuts off in the disease name
            if (lastMatchIndex != field.length()) {
                boolean foundFieldMatch = false;

                final Matcher m2 = Pattern.compile("(\\w[\\w\\s\\/()]*):?\\s+(.+)\\s*-*").matcher(field.substring(lastMatchIndex));

                if (m2.matches()) {
                    for (int i = 0; i < diseaseStrings.size() && !foundFieldMatch; i++) {
                        if (fieldStrings.get(i).equals(m2.group(2))) {
                            // only add if the disease isn't a duplicate
                            boolean foundDisease = false;
                            final String[] split = diseaseStrings.get(i).split("/");
                            for (final String s : split) {
                                if (s.equals(m2.group(1))) {
                                    foundDisease = true;
                                }
                            }
                            if ( !foundDisease) {
                                diseaseStrings.set(i, diseaseStrings.get(i) + "/" + m2.group(1));
                            }
                            foundFieldMatch = true;
                        }
                    }

                    // didn't find a matching field value, add a new one
                    if ( !foundFieldMatch) {
                        diseaseStrings.add(m2.group(1));
                        fieldStrings.add(m2.group(2));
                    }
                } else {
                    System.err.println("## Didn't match:\t" + field.substring(lastMatchIndex));
                }
            }

            // compile string collections into one string
            String str = "";
            for (int i = 0; i < diseaseStrings.size(); i++) {
                str += "<p><b>" + diseaseStrings.get(i) + "</b>: " + fieldStrings.get(i) + "</p";
            }
            return str;
        } else {
            return field;
        }
    }

    
    
    /**
     * 
     * @author mcmatt
     *
     */
    private class ChooseFormStructDialog extends JDialog implements ActionListener {
        private static final long serialVersionUID = 4199199899439094828L;

        private final PlugInDialogBRICS_Mapper owner;

        private LocalTableModel structsModel;

        private JTable structsTable;

        private final ArrayList<String> descAL 		= new ArrayList<String>();
        private final ArrayList<String> shortNameAL = new ArrayList<String>();
        private final ArrayList<String> versionAL 	= new ArrayList<String>();
        private final ArrayList<String> statusAL 	= new ArrayList<String>();
        private final ArrayList<String> diseaseAL 	= new ArrayList<String>();

        private JScrollPane structsScrollPane;

        public ChooseFormStructDialog(final PlugInDialogBRICS_Mapper owner) {
            super(owner, true);
            this.owner = owner;

            init();
        }

        private void init() {
            setTitle("Choose Form Structure");
            final int numColumns = 5;
            final String[] columnNames = {"Name", "Description", "Version", "Status", "Disease"};
            structsModel = new LocalTableModel();
            structsTable = new JTable(structsModel) {
                private static final long serialVersionUID = 3053232611901005303L;

                @Override
                public String getToolTipText(final MouseEvent e) {
                    String tip = "";

                    final java.awt.Point p = e.getPoint();
                    final int rowIndex = rowAtPoint(p);
                    final int colIndex = columnAtPoint(p);

                    tip = (String) structsModel.getValueAt(rowIndex, colIndex);
                    return tip;

                }
            };

            structsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

            for (final String element : columnNames) {
                structsModel.addColumn(element);
            }

            structsTable.getColumn("Name").setMinWidth(150);
            structsTable.getColumn("Description").setMinWidth(300);
            
            structsTable.getColumn("Version").setMinWidth(35);
            structsTable.getColumn("Version").setMaxWidth(55);
            structsTable.getColumn("Version").setPreferredWidth(50);
            
            structsTable.getColumn("Status").setMinWidth(35);
            structsTable.getColumn("Status").setMaxWidth(85);
            structsTable.getColumn("Status").setPreferredWidth(110);

            // new way of doing web service
            for (final FormStructure fs : formStructureList) {
                if (fs.getShortName().equals("")) {
                    // something is wrong. a shortname is required. this is to work around an apparent stage DDT problem
                    continue;
                }
                final String desc 		= fs.getDescription();
                final String shortname 	= fs.getShortName();
                final String version 	= fs.getVersion().toString();
                final String status 	= fs.getStatus().toString();
                final String disease 	= fs.getDiseaseStructureString();

                descAL.add(desc);
                shortNameAL.add(shortname);
                versionAL.add(version);
                statusAL.add(status);
                diseaseAL.add(disease);
            }

            // make sure we found a structure for imaging
            if (shortNameAL.size() == 0) {
                displayWarning("No structures were found in the data dictionary.");
                return;
            }

            // before sorting, remove structs that are not the most current
            for (int i = 0; i < shortNameAL.size(); i++) {
                for (int j = i + 1; j < shortNameAL.size(); j++) {
                    // check for multiple versions of the same struct
                    if (shortNameAL.get(i).equalsIgnoreCase(shortNameAL.get(j))) {
                        // remove any later struct with a lower version number
                        if (Integer.parseInt(versionAL.get(i)) > Integer.parseInt(versionAL.get(j))) {
                            shortNameAL.remove(j);
                            descAL.remove(j);
                            versionAL.remove(j);
                            statusAL.remove(j);
                            diseaseAL.remove(j);
                        } else {
                            shortNameAL.remove(i);
                            descAL.remove(i);
                            versionAL.remove(i);
                            statusAL.remove(i);
                            diseaseAL.remove(i);
                        }
                    }
                }
            }

            final TreeSet<String> sortedNamesSet = new TreeSet<String>(new AlphabeticalComparator());
            for (int i = 0; i < shortNameAL.size(); i++) {
                sortedNamesSet.add(shortNameAL.get(i));
            }

            final Object[] rowData = new Object[numColumns];
            final Iterator<String> iter = sortedNamesSet.iterator();

            while (iter.hasNext()) {
                final String name = iter.next();

                for (int i = 0; i < shortNameAL.size(); i++) {
                    if (name.equals(shortNameAL.get(i))) {
                        rowData[0] = shortNameAL.get(i);
                        rowData[1] = descAL.get(i);
                        rowData[2] = versionAL.get(i);
                        rowData[3] = statusAL.get(i);
                        rowData[4] = diseaseAL.get(i);
                        structsModel.addRow(rowData);

                        break;
                    }
                }
            }

            structsTable.setRowSelectionInterval(0, 0);
            structsTable.setAutoCreateRowSorter(true);
            structsTable.getRowSorter().toggleSortOrder(0);

            structsScrollPane = new JScrollPane(structsTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

            structsScrollPane.setPreferredSize(new Dimension(800, 300));

            final JPanel OKPanel = new JPanel();
            final JButton OKButton = new JButton("Select");
            OKButton.setActionCommand("ChooseStructOK");
            OKButton.addActionListener(this);
            OKButton.setMinimumSize(defaultButtonSize);
            OKButton.setPreferredSize(defaultButtonSize);
            OKButton.setFont(serif12B);
            OKPanel.add(OKButton);

            getContentPane().add(structsScrollPane, BorderLayout.CENTER);
            getContentPane().add(OKPanel, BorderLayout.SOUTH);

            pack();

            //this.setMinimumSize(this.getSize());
            //this.setSize(new Dimension(owner.getSize().width, this.getSize().height));
            centerInWindow(owner, this);

            setVisible(true);
        }

        /**
         * action performed
         */
        @Override
        public void actionPerformed(final ActionEvent e) {
            final String command = e.getActionCommand();
            if (command.equalsIgnoreCase("ChooseStructOK")) {
                final int selectedRow = structsTable.getSelectedRow();
                if (selectedRow != -1) {
                    this.dispose();
                    final String fsName = (String) structsModel.getValueAt(selectedRow, 0);
                    new populateBRICSStructureTable(owner, fsName);
                }
            }
        }

        /**
         * This inner class is used to sort the list by instance number
         */
        private class AlphabeticalComparator implements Comparator<String> {
            @Override
            public int compare(final String a, final String b) {
                return (a.toLowerCase().compareTo(b.toLowerCase()));

            }
        }
    }
    
    
    /**
     * Gets data elements for the selected form and copies them to the BRICS reference data element table
     * 
     */
    private class populateBRICSStructureTable {
        private static final long serialVersionUID = 859201819000159789L;
 
        //private FormStructure formStructure;
           
		/**
		 * Gets data elements for the selected form and copies them to the BRICS reference data element table
		 * @param owner	- main GUI
		 * @param fsName - Form structure name for the selected form structure.
		 */
        public populateBRICSStructureTable(final PlugInDialogBRICS_Mapper owner, final String fsName) {
            
            // Goes and gets the selected form structure
        	final FormDataElementsRESTThread thread = new FormDataElementsRESTThread(owner, fsName, true);
            thread.run();
            formStructure = thread.getFullFormStructure();
 
            if (formStructure == null) {
                displayError("Form structure not found in Data Dictionary: " + fsName);
                dispose();
                return;
            }

            copyFormStructureToTable();
            listPane.setBorder(buildTitledBorder("  Reference Form Structure:  " + fsName));
            loadCSVButton.setEnabled(true);
        }
        
        /**
         * Copies the selected form structure data elements into the BRICS reference data element table
         */
        private void copyFormStructureToTable() {
        	
        	deTableModel.setRowCount(formStructure.getDataElements().size());
        	
        	// Since repeatable groups doesn't provide the groups in order that they are presented in the actual form
        	// we calculate the group position to properly display in the correct order in the table
        	// rowPos will contain indexes to the starting rows in the table for each group
        	int[] rowPos = new int[formStructure.getRepeatableGroups().size()]; 
        	for (final RepeatableGroup group : formStructure.getRepeatableGroups() ) {	
	        	rowPos[group.getPosition()]= group.getSize();
        	}
        	
        	for (int j = rowPos.length-1; j >= 0; j--) {
        		int sum = 0;
        		for (int i =0; i < j; i++) {
        			sum = sum + rowPos[i];
        		}
        		rowPos[j]=sum;
        	}
        	
        	
        	FormStructureData fsInfo = new FormStructureData(formStructure);
        	for (final RepeatableGroup group : formStructure.getRepeatableGroups() ) {
        		int i=0;
	        	for (final MapElement mapde : group.getDataElements()) {
	        		DataElement de = fsInfo.getDataElement(mapde.getStructuralDataElement());
	     		
	        		int row = rowPos[group.getPosition()]+i;

	        		deTableModel.setValueAt(group.getName(), row, deTableModel.getColumnIndex("Group"));	
	        		deTableModel.setValueAt(mapde.getStructuralDataElement().getName(), row, deTableModel.getColumnIndex("Element Name"));			
	        		deTableModel.setValueAt(de.getTitle(), row, deTableModel.getColumnIndex("Title"));											
	        		
	        		deTableModel.setValueAt(de.getType(), row, deTableModel.getColumnIndex("Type"));	
	        		
	        		String strPVs = new String();
	                for (ValueRange vr : de.getValueRangeList() ) {
	                	strPVs += vr.getValueRange() + "; ";
	                }
	        		deTableModel.setValueAt(strPVs, row, deTableModel.getColumnIndex("Reference PVs"));  // PV list
	        		
	        		JLabel l = new JLabel(mapde.getRequiredType().toString());
	                l.setFont(serif12);

	        		deTableModel.setValueAt(mapde.getRequiredType().toString(), row, deTableModel.getColumnIndex("Required"));  // Required DE or not
	        		
	        		//System.out.println(" DE URI " + de.getUri());  //URI seems problematic. - maybe because on staging or demo
	        		//System.out.println(" FS URI " + formStructure.getUri());  //URI seems problematic. - maybe because on staging or demo
	        		
	        		i++;
	        	}
        	}    	     	
        }
    }

    /**
     * 
     * @return
     */
    private boolean readCSVFile() {
        BufferedReader br = null;

        try {
            String str;
            final FileInputStream fis = new FileInputStream(csvFile);
            br = new BufferedReader(new InputStreamReader(fis));

            // first line is data element attributes Name, PVs, PV Descriptions, Type, Title - on the first two are required.
            str = br.readLine();
            String[] DEHeaders = str.split(CSV_OUTPUT_DELIM);
            for(int i =0; i < DEHeaders.length; i++) {
            	DEHeaders[i] = DEHeaders[i].trim();
            }
            
            final String deName  = DEHeaders[0].trim();
            final String deType  = DEHeaders[1].trim();
            final String dePVs   = DEHeaders[2].trim();
            if ( !deName.toLowerCase().equals("name") || !dePVs.toLowerCase().equals("pvs") ||  !deType.toLowerCase().equals("type")) {
            	printlnToLog("Source data elements are not in proper format - Header row: Name, Type, PVs, PV Descriptions, and Title where only Name, Type, and PVs are required.");
            	return false;
            }

            Vector<String[]> dataElements = new Vector<String[]>(40);
            dataElements.add(DEHeaders);
            int row = 1;
            while ( (str = br.readLine()) != null) {
                str = str.trim();
                String[] deDef = str.split(CSV_OUTPUT_DELIM);
                for(int i =0; i < deDef.length; i++) {
                	deDef[i] = deDef[i].trim();
                }
                if (!deDef[2].isEmpty()) { 			// PVs not empty
                	if ( !deDef[2].matches("^[a-zA-Z0-9;]*$") ) {
                		 displayError("PVs can only contain a-z, A-Z and 0-9 separated by semicolons." + "  Row: " + row + " Element: " + deDef[0]);
                		 fis.close();
                	}
                	else if (deDef[2].endsWith(";")) {
                		deDef[2] = deDef[2].substring(0, deDef[2].length()-1);
                	}
                }
                dataElements.add(deDef);
                row++;
            }
            fis.close();
            //Open dialog with and populate with source DEs
            SourceDEsDialog csvDEDialog = new SourceDEsDialog(this, dataElements);
        } catch (final Exception e) {
        	//e.printStackTrace();
        } finally {
            if (br != null) {
                try {
                    br.close();
                } catch (IOException e) {
                    System.err.println("Problem closing CSV file handle.");
                    e.printStackTrace();
                }
                return false;
            }
        }
         
        
        return true;      
    }
    
    /**
     *  MATT   - Working!!!!
     * 
     * 
     */
    private class SourceDEsDialog extends JDialog implements ActionListener {
        private static final long serialVersionUID = 859201819000159789L;

        private final PlugInDialogBRICS_Mapper owner;

        Vector<String[]> dataElements;
        JTable 			 srcDETable;
        LocalTableModel	 srcDETableModel;
        JScrollPane		 srcDEPane;


        public SourceDEsDialog(final PlugInDialogBRICS_Mapper owner, final Vector<String[]> dataElements) {
            super(owner, false);

            this.owner = owner;
            this.dataElements = dataElements;
            if (dataElements == null) {
                displayError("Source data elements vector is empty");
                dispose();
                return;
            }

            init();
        }

        /**
         * 
         */
        private void init() {
        	setTitle("Source Data Elements");
            final JPanel mainPanel = new JPanel(new GridBagLayout());

            final GridBagConstraints gbc = new GridBagConstraints();

            try {
                setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
            } catch (final Exception e) {
                // setIconImage() is not part of the Java 1.5 API - catch any
                // runtime error on those systems
            }

            final JPanel OKPanel = new JPanel();

            final JButton closeButton = new JButton("Close");
            closeButton.setActionCommand("srcDEClose");
            closeButton.addActionListener(this);
            closeButton.setMinimumSize(defaultButtonSize);
            closeButton.setPreferredSize(defaultButtonSize);
            closeButton.setFont(serif12B);
            
            final JButton clearButton = new JButton("Clear");
            clearButton.setActionCommand("srcDEClear");
            clearButton.addActionListener(this);
            clearButton.setMinimumSize(defaultButtonSize);
            clearButton.setPreferredSize(defaultButtonSize);
            clearButton.setFont(serif12B);
            
            final JButton pairRButton = new JButton(" Pair ");
            pairRButton.setActionCommand("srcDEPairR");
            pairRButton.addActionListener(this);
            pairRButton.setMinimumSize(new Dimension(130, 30));
            pairRButton.setPreferredSize(new Dimension(130, 30));
            pairRButton.setFont(serif12B);
            pairRButton.setToolTipText("Copies source data element to reference data element");
            
            final JButton pairAButton = new JButton("Pair: Append");
            pairAButton.setActionCommand("srcDEPairA");
            pairAButton.addActionListener(this);
            pairAButton.setMinimumSize(new Dimension(130, 30));
            pairAButton.setPreferredSize(new Dimension(130, 30));
            pairAButton.setFont(serif12B);
            pairAButton.setToolTipText("Appends source data element to reference data element");
            
            OKPanel.add(pairRButton);
            OKPanel.add(pairAButton);
            OKPanel.add(clearButton);
            OKPanel.add(closeButton);

            gbc.weightx = 0;
            gbc.weighty = 0;
            gbc.gridx = 0;
            gbc.gridy = 3;
            gbc.insets = new Insets(10, 5, 10, 5);
            gbc.gridwidth = 1;
            mainPanel.add(OKPanel, gbc);

            gbc.gridy = 2;
            gbc.weightx = 1;
            gbc.weighty = 1;
            gbc.fill = GridBagConstraints.BOTH;
            JScrollPane sPane = buildCSVPanel();
            mainPanel.add(sPane, gbc);
            getContentPane().add(mainPanel, BorderLayout.CENTER);

            final Dimension dim = getContentPane().getPreferredSize();
            if (dim.height > 500) {
                dim.height = 500;
            }
            sPane.setPreferredSize(dim);
  
            pack();
            centerInWindow(owner, this);
           
            setVisible(true);
        }
        
        /**
         * 
         * @return
         */
        private JScrollPane buildCSVPanel() {
        	srcDETableModel = new LocalTableModel();
        	
        	//Columns defined by first row in CSV file
        	srcDETableModel.addColumn("Mapped");
        	for (int i = 0; i < dataElements.get(0).length; i++) {
        		srcDETableModel.addColumn(dataElements.get(0)[i].trim());
        	}

        	
        	srcDETable = new JTable(srcDETableModel);
            srcDETable.setPreferredScrollableViewportSize(new Dimension(850, 300));
            srcDETable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            srcDETable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
            srcDETable.setAutoCreateRowSorter(true);
            //srcDETable.getColumn("Name").setCellRenderer(new CellRenderer());
            
            srcDEPane = new JScrollPane(srcDETable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                    JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            srcDEPane.setBorder(buildTitledBorder(" Source Data Elements"));  
            populateSrcTable();
            updateMappedColumn();
            return srcDEPane;
        }
        
        private class CellRenderer extends DefaultTableCellRenderer {

        	@Override
            public Component getTableCellRendererComponent(final JTable table, final Object value,
                    final boolean isSelected, final boolean hasFocus, final int row, final int col) {
        		
                final Component cell = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col);
                
                TableCellRenderer refRenderer = table.getCellRenderer(row, col);
                if (refRenderer instanceof CellRenderer) { 
    	
 
    	            if (col == 0  ) {
    	            	setBackground(Color.yellow);
    	            } 
    	            else {
    	                setBackground(Color.white);
    	            }
                }
                return cell;
        	}
        }
        
        /**
         * 
         */
        private void populateSrcTable() {
        	
        	if (dataElements == null) return;
        	
        	srcDETableModel.setRowCount(dataElements.size()-1);
            //System.out.println("Hey3" + " row = " + dataElements);  	
        	
            for (int i = 1; i < dataElements.size(); i++) {
	        	if (dataElements.get(i).length > 0) {
	        		srcDETableModel.setValueAt(dataElements.get(i)[0], i-1, srcDETableModel.getColumnIndex("Name"));				
	        	}
	        	if (dataElements.get(i).length > 1) {
	        		srcDETableModel.setValueAt(dataElements.get(i)[1], i-1, srcDETableModel.getColumnIndex("Type"));
	        	}
	        	if (dataElements.get(i).length > 2) {
	        		srcDETableModel.setValueAt(dataElements.get(i)[2], i-1, srcDETableModel.getColumnIndex("PVs"));
	        	}
	        	if (dataElements.get(i).length > 3) {
	        		srcDETableModel.setValueAt(dataElements.get(i)[3], i-1, srcDETableModel.getColumnIndex("PV Descriptions"));
	        	}
	        	if (dataElements.get(i).length > 4) {
	        		srcDETableModel.setValueAt(dataElements.get(i)[4], i-1, srcDETableModel.getColumnIndex("Title"));
	        	}
        	}
        }
        
        
        /**
         * 
         */
        private void updateMappedColumn() {
        	
        	// Clear out 
        	for(int i = 0; i < srcDETable.getRowCount(); i++) {
        		srcDETableModel.setValueAt("", i, srcDETableModel.getColumnIndex("Mapped"));
        	}
        	
            for(int i = 0; i < srcDETable.getRowCount(); i++) {
            	String srcName = (String)srcDETable.getValueAt(i, srcDETableModel.getColumnIndex("Name"));

            	for (int j=0; j < deTable.getRowCount();  j++) {
            		
            		String names = (String)deTable.getValueAt(j, deTableModel.getColumnIndex("Source Name"));
            		if (names != null) {
	            		String[] nameDef = names.split(";");
	            		
	            		for(int k = 0; k < nameDef.length; k++) {
			            	nameDef[k] = nameDef[k].trim();
			            	if ( nameDef[k].equals(srcName) ) {
		            			srcDETableModel.setValueAt(" Yes", i, srcDETableModel.getColumnIndex("Mapped"));
		            			break;
		            		}
			            	
	            		}
            		}
                }
            }

	    }
        
        
        /**
         * action performed
         */
        @Override
        public void actionPerformed(final ActionEvent e) {
            final String command = e.getActionCommand();

            if (command.equalsIgnoreCase("srcDEClose")) {
                dispose();
            } 
            if (command.equalsIgnoreCase("srcDEClear")) {
            	if (deTable.getSelectedRow() != -1) {
	            	deTable.setValueAt("", deTable.getSelectedRow(), deTableModel.getColumnIndex("Source Name"));
	                deTable.setValueAt("", deTable.getSelectedRow(), deTableModel.getColumnIndex("Source Type"));
	                deTable.setValueAt("",  deTable.getSelectedRow(), deTableModel.getColumnIndex("Source PVs"));
            	}
            } 
            if (command.equalsIgnoreCase("srcDEPairR")) {

            	if (deTable.getSelectedRow() != -1 && srcDETable.getSelectedRow() != -1) {
            		final int rowRef = deTable.getSelectedRow();

            		if (  !((String)(deTable.getValueAt(rowRef, deTableModel.getColumnIndex("Reference PVs")))).isEmpty() &&
            			   ((String)(srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("PVs")))).isEmpty() ) {
       	            		displayWarning("Source PVs cell is empty.");
       	            		return;
                   	}

	                deTable.setValueAt(srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("Name")), rowRef, deTableModel.getColumnIndex("Source Name"));
	                deTable.setValueAt(srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("Type")), rowRef, deTableModel.getColumnIndex("Source Type"));
	                deTable.setValueAt(srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("PVs")),  rowRef, deTableModel.getColumnIndex("Source PVs"));
            	}
            	else {
            		displayWarning("Please select both reference and source data elements.");
            	}
            }
            if (command.equalsIgnoreCase("srcDEPairA")) {
            	if (deTable.getSelectedRow() != -1 && srcDETable.getSelectedRow() != -1) {
	                final int rowRef = deTable.getSelectedRow();
	                  
	                if (  !((String)(deTable.getValueAt(rowRef, deTableModel.getColumnIndex("Reference PVs")))).isEmpty() &&
	            			   ((String)(srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("PVs")))).isEmpty() ) {
	       	            		displayWarning("Source PVs cell is empty.");
	       	            		return;
	                }
	                
	                String refPVStr = (String)deTable.getValueAt(rowRef, deTableModel.getColumnIndex("Source PVs")); 							// Get mapping source PVs
	                String scrPVStr = (String)srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("PVs"));
	                //System.out.println("refPVStr = " + refPVStr + " scrPVStr = " + scrPVStr);
	                if (scrPVStr != null ) 
	                	scrPVStr.trim();

	                if ( refPVStr != null) {
	                	refPVStr.trim(); 
	                	if (refPVStr.equals(scrPVStr)) {
	                		// do nothing;
	                	}
	                	else if (refPVStr.isEmpty()) {
	                		deTable.setValueAt(scrPVStr, rowRef, deTableModel.getColumnIndex("Source PVs"));	// Append PVs
	                		deTable.setValueAt("", rowRef, deTableModel.getColumnIndex("PV Mappings"));  // Clear out the mapped PVs since source PVs changed
	                	}
	                	else {
	                		deTable.setValueAt(refPVStr +";" + scrPVStr, rowRef, deTableModel.getColumnIndex("Source PVs"));	// Append PVs
	                		deTable.setValueAt("", rowRef, deTableModel.getColumnIndex("PV Mappings"));  // Clear out the mapped PVs since source PVs changed
	                	}
	                }
	                else {
	                	deTable.setValueAt(srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("Name")), rowRef, deTableModel.getColumnIndex("Source Name"));
	                	deTable.setValueAt(srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("Type")), rowRef, deTableModel.getColumnIndex("Source Type"));
	                	deTable.setValueAt(scrPVStr, rowRef, deTableModel.getColumnIndex("Source PVs")); 
	                }
	                
	                if (deTable.getValueAt(rowRef, deTableModel.getColumnIndex("Source Name")) != null && 
	                		srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("Name")) != null) {
	                	if (((String)deTable.getValueAt(rowRef, deTableModel.getColumnIndex("Source Name"))).isEmpty()) {
		                	
		                	deTable.setValueAt(srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("Name")), rowRef, deTableModel.getColumnIndex("Source Name")); 
	                	}
	                	else if (!((String)deTable.getValueAt(rowRef, deTableModel.getColumnIndex("Source Name"))).equals(srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("Name")))) {
		                		deTable.setValueAt(deTable.getValueAt(rowRef, deTableModel.getColumnIndex("Source Name")) + ";" + 
		                				srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("Name")), rowRef, deTableModel.getColumnIndex("Source Name"));
	                	}
	                	deTable.setValueAt(srcDETable.getValueAt(srcDETable.getSelectedRow(), srcDETableModel.getColumnIndex("Type")), rowRef, deTableModel.getColumnIndex("Source Type")); 
	                }
            	}
            	else {
            		displayWarning("Please select both reference and source data elements.");
            	}        		
            }
            if (deTable.getSelectedRow() != -1) {
            	deTable.setValueAt("",  deTable.getSelectedRow(), deTableModel.getColumnIndex("PV Mappings"));
            }
            updateMappedColumn();
            validateRequired();
        }
    }
    
    
    /**
     *  
     * 
     */
    private class PVMappingDialog extends JDialog implements ActionListener {
        private static final long serialVersionUID = 859201819000159789L;

        private final PlugInDialogBRICS_Mapper owner;

        LocalTableModel	 srcDETableModel;
        JScrollPane		 srcDEPane;


        public PVMappingDialog(final PlugInDialogBRICS_Mapper owner) {
            super(owner, false);

            this.owner = owner;
            init();
        }

        /**
         * 
         */
        private void init() {
        	setTitle("Map Permissible Values");
            final JPanel mainPanel = new JPanel(new GridBagLayout());

            final GridBagConstraints gbc = new GridBagConstraints();

            try {
                setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
            } catch (final Exception e) {
                // setIconImage() is not part of the Java 1.5 API - catch any
                // runtime error on those systems
            }

            final JPanel OKPanel = new JPanel();

            final JButton cancelButton = new JButton("Cancel");
            cancelButton.setActionCommand("Cancel");
            cancelButton.addActionListener(this);
            cancelButton.setMinimumSize(defaultButtonSize);
            cancelButton.setPreferredSize(defaultButtonSize);
            cancelButton.setFont(serif12B);
            
            final JButton pairButton = new JButton("Done");
            pairButton.setActionCommand("Done");
            pairButton.addActionListener(this);
            pairButton.setMinimumSize(defaultButtonSize);
            pairButton.setPreferredSize(defaultButtonSize);
            pairButton.setFont(serif12B);
            pairButton.setToolTipText("Done");
            
            OKPanel.add(pairButton);
            OKPanel.add(cancelButton);

            gbc.weightx = 0;
            gbc.weighty = 0;
            gbc.gridx = 0;
            gbc.gridy = 3;
            gbc.insets = new Insets(10, 5, 10, 5);
            gbc.gridwidth = 1;
            mainPanel.add(OKPanel, gbc);

         
            gbc.gridy = 2;
            gbc.weightx = 1;
            gbc.weighty = 1;
            gbc.fill = GridBagConstraints.BOTH;
            //JScrollPane sPane = buildPVPanel();
            //mainPanel.add(sPane, gbc);

            getContentPane().add(mainPanel, BorderLayout.CENTER);

            final Dimension dim = getContentPane().getPreferredSize();
            if (dim.height > 500) {
                dim.height = 500;
            }
            //sPane.setPreferredSize(dim);

            pack();
            centerInWindow(owner, this);
           
            setVisible(true);
        }
        
        private void buildPVPanel() {
        	
        	
        	
        	
        }
        
        /**
         * 
         */
        private void populateSrcTable() {
        	
        	//if (dataElements == null) return;
        	
        	//srcDETableModel.setRowCount(dataElements.size()-1);
        	   	
        	/**for (int i = 1; i < dataElements.size(); i++) {
	        	if (dataElements.get(i).length > 0) {
	        		srcDETableModel.setValueAt(dataElements.get(i)[0], i-1, 0);				
	        	}
        	}*/
        	
        }
        
        
        /**
         * action performed
         */
        @Override
        public void actionPerformed(final ActionEvent e) {
            final String command = e.getActionCommand();

            if (command.equalsIgnoreCase("Cancel")) {
                dispose();
            } 
            if (command.equalsIgnoreCase("Done")) {
            	
            } 
        }
    }
    
    
    /**
     *  Editor for Source PV.
     * 
     */
    private class srcPVDialog extends JDialog implements ActionListener {
        private static final long serialVersionUID = 859201819000159789L;

        private final PlugInDialogBRICS_Mapper owner;
        private JTextField pvsTF = null;


        public srcPVDialog(final PlugInDialogBRICS_Mapper owner) {
            super(owner, false);

            this.owner = owner;
            init();
        }

        /**
         * 
         */
        private void init() {
        	setTitle("Edit Source DE Permissible Values");
            final JPanel mainPanel = new JPanel(new GridBagLayout());

            final GridBagConstraints gbc = new GridBagConstraints();

            try {
                setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
            } catch (final Exception e) {

            }

            final JPanel OKPanel = new JPanel();

            final JButton cancelButton = new JButton("Cancel");
            cancelButton.setActionCommand("Cancel");
            cancelButton.addActionListener(this);
            cancelButton.setMinimumSize(defaultButtonSize);
            cancelButton.setPreferredSize(defaultButtonSize);
            cancelButton.setFont(serif12B);
            
            final JButton pairButton = new JButton("Done");
            pairButton.setActionCommand("Done");
            pairButton.addActionListener(this);
            pairButton.setMinimumSize(defaultButtonSize);
            pairButton.setPreferredSize(defaultButtonSize);
            pairButton.setFont(serif12B);
            pairButton.setToolTipText("Done");
            
            OKPanel.add(pairButton);
            OKPanel.add(cancelButton);

            gbc.weightx = 0;
            gbc.weighty = 0;
            gbc.gridx = 0;
            gbc.gridy = 3;
            gbc.insets = new Insets(10, 5, 10, 5);
            gbc.gridwidth = 1;
            mainPanel.add(OKPanel, gbc);

            pvsTF = new JTextField((String)deTableModel.getValueAt(deTable.getSelectedRow(), 8));
            pvsTF.setMinimumSize(new Dimension(300, 30));
            pvsTF.setPreferredSize(new Dimension(300, 30));
            gbc.gridy = 2;
            gbc.weightx = 1;
            gbc.weighty = 1;
            gbc.fill = GridBagConstraints.BOTH;
            mainPanel.add(pvsTF, gbc);

            getContentPane().add(mainPanel, BorderLayout.CENTER);

            pack();
            centerInWindow(owner, this);
           
            setVisible(true);
        }
             
        
        /**
         * action performed
         */
        @Override
        public void actionPerformed(final ActionEvent e) {
            final String command = e.getActionCommand();

            if (command.equalsIgnoreCase("Cancel")) {
                dispose();
            } 
            if (command.equalsIgnoreCase("Done")) {
            	String str = pvsTF.getText();
            	 if (str.matches("^[a-zA-Z0-9;]*$") ) {
            		 str = pvsTF.getText();
            		 if (str.endsWith(";")) {
            			 str = str.substring(0, str.length()-1);
                 	 }
            		 deTableModel.setValueAt(str, deTable.getSelectedRow(), 8);
            		 dispose();
            	 }
            	 else {
            		 displayError("PVs can only contain a-z, A-Z and 0-9 separated by semicolons.");
            	 }
            	
            } 
        }
    }

 
    /**
     * Filter out Imaging and Archived form structures leaving only CLINCICAL form structures
     * @param fullList
     * @return
     */
    private List<FormStructure> filterDataStructures(final List<FormStructure> fullList) {
        final List<FormStructure> filteredList = new ArrayList<FormStructure>();

        for (final FormStructure ds : fullList) {
            if (ds.getShortName().equals("")) {
                // something is wrong. a shortname is required. this is to work around an apparent stage DDT problem
                continue;
            }

            if (ds.getFileType().equals(SubmissionType.IMAGING)) {
                // only include non-archived structures
                if ( !ds.getStatus().equals(StatusType.ARCHIVED)) {
                    filteredList.add(ds);
                }
            }
        }

        // Returning clinical form structures
        return filteredList;
    }


    
    /**
     * Connects to BRICS data dictionary web service (via RESTful API) to retrieve the list of form
     * structures (without their attached data elements).
     */
    public class FormListRESTThread extends Thread implements ActionListener {
        private static final String ddAuthBase = "/portal/ws/webstart/dictionary/formStructure/details";

        private static final String ddRequestBase = "/portal/ws/ddt/dictionary/FormStructure";

        // private static final String ddStructListRequest = ddRequestBase + "/Published/list?type=CLINICAL";
        //private static final String ddStructListRequest = ddRequestBase + "/Published/list?type=CLINICAL&incDEs=false";
        private static final String ddStructListRequest = ddRequestBase + "/Published/list?type=IMAGING&incDEs=false";

        private JButton progressCancelButton;

        private final PlugInDialogBRICS_Mapper parent;

        public FormListRESTThread(final PlugInDialogBRICS_Mapper parent) {
            super();
            this.parent = parent;
        }

        @Override
        public void run() {
            ViewJProgressBar progressBar = null;
            try {
                progressBar = new ViewJProgressBar("BRICS", "Retrieving form structures from BRICS data dictionary...", 0, 100, true);
                progressBar.setVisible(true);
                progressBar.updateValue(20);
                progressBar.setIndeterminate(true);
                progressCancelButton = progressBar.getCancelButton();
                progressCancelButton.addActionListener(this);

                // should have already read in the config file (moved from here to be before the GUI init() call)

                WebClient client;
                if (ddUseAuthService) {
                    client = WebClient.create(ddServerURL + ddAuthBase);
                } else {
                    client = WebClient.create(ddServerURL + ddStructListRequest);
                }

                final HTTPConduit conduit = WebClient.getConfig(client).getHttpConduit();
                conduit.getClient().setReceiveTimeout(0);

                if ( !ddAuthUser.equals("") && !ddAuthPass.equals("")) {
                    client.header("userName", ddAuthUser);
                    client.header("pass", ddAuthPass);
                }

                final long startTime = System.currentTimeMillis();
                List<FormStructure> fullList;
                if (ddUseAuthService) {
                    fullList = (List<FormStructure>) client.accept("text/xml").getCollection(FormStructure.class);
                } else {
                    final DataStructureList dsl = client.accept("text/xml").get(DataStructureList.class);
                    fullList = dsl.getList();
                }
                final long endTime = System.currentTimeMillis();

                System.out.println("Webservice request (sec):\t" + ( (endTime - startTime) / 1000));

                formStructureList = filterDataStructures(fullList);

                // for (final FormStructure ds : dataStructureList) {
                // System.out.println("FS title:\t" + ds.getTitle() + "\tversion:\t" + ds.getVersion() + "\tpub:\t" +
                // ds.getStatus());
                // }

                progressBar.updateValue(100);
                progressBar.setVisible(false);
                progressBar.dispose();
                printlnToLog("Successful retrieval of form structures.");

                selectStructButton.setEnabled(true);
            } catch (final Exception e) {
                e.printStackTrace();
                if (progressBar != null) {
                    progressBar.setVisible(false);
                    progressBar.dispose();
                    displayError("Error in connecting to web service");
                    parent.dispose();
                    if (JDialogStandalonePlugin.isExitRequired()) {
                        System.gc();
                        System.exit(0);
                    }
                }
            }
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            if (e.getSource() == progressCancelButton) {
                dispose();
                if (JDialogStandalonePlugin.isExitRequired()) {
                    System.gc();
                    System.exit(0);
                }
            }
        }
    }

    /**
     * Class that connects to BRICS data dictionary web service (via RESTful API) to retrieve the data elements for a
     * given form structure.
     */
    private class FormDataElementsRESTThread extends Thread implements ActionListener {

        private static final String ddRequestBase = "/portal/ws/ddt/dictionary/FormStructure";

        private JButton progressCancelButton;

        private final PlugInDialogBRICS_Mapper parent;

        private final String formStructName;

        private FormStructure fullFormStructure;

        private final boolean addProgressBar;
        
        /**
         * 
         * @param parent
         * @param formStructName
         * @param addProgressBar
         */
        public FormDataElementsRESTThread(final PlugInDialogBRICS_Mapper parent, final String formStructName, final boolean addProgressBar) {
            super();
            this.parent = parent;
            this.formStructName = formStructName;
            this.addProgressBar = addProgressBar;
        }

        @Override
        public void run() {
            ViewJProgressBar progressBar = null;
            try {
                if (addProgressBar) {
                    progressBar = new ViewJProgressBar("BRICS", "Retrieving data elements for form structure: " + formStructName, 0, 100, true);
                    progressBar.setVisible(true);
                    progressBar.updateValue(20);
                    progressBar.setIndeterminate(true);
                    progressCancelButton = progressBar.getCancelButton();
                    progressCancelButton.addActionListener(this);
                }

                // should have already read in the config file (moved from here to be before the GUI init() call)

                WebClient client;
                client = WebClient.create(ddServerURL + ddRequestBase + "/" + formStructName);

                final HTTPConduit conduit = WebClient.getConfig(client).getHttpConduit();
                conduit.getClient().setReceiveTimeout(0);

                final long startTime = System.currentTimeMillis();
                fullFormStructure = client.accept("text/xml").get(FormStructure.class);
                final long endTime = System.currentTimeMillis();

                System.out.println("Webservice request (sec):\t" + ( (endTime - startTime) / 1000));

                for (int i = 0; i < formStructureList.size(); i++) {
                    final FormStructure curFS = formStructureList.get(i);
                    if (curFS.getShortName().equals(fullFormStructure.getShortName())) {
                    	formStructureList.set(i, fullFormStructure);
                        break;
                    }
                }

                // for (final FormStructure ds : dataStructureList) {
                // System.out.println("FS title:\t" + ds.getTitle() + "\tversion:\t" + ds.getVersion() + "\tpub:\t"
                // +
                // ds.getStatus());
                // }

                if (addProgressBar) {
                    progressBar.updateValue(100);
                    progressBar.setVisible(false);
                    progressBar.dispose();
                }
                printlnToLog("Successful retrieval of data elements for form structure: " + formStructName);
            } catch (final Exception e) {
                e.printStackTrace();
                if (progressBar != null) {
                    progressBar.setVisible(false);
                    progressBar.dispose();
                    displayError("Error in connecting to web service");
                    parent.dispose();
                    if (JDialogStandalonePlugin.isExitRequired()) {
                        System.gc();
                        System.exit(0);
                    }
                }
            }
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            if (e.getSource() == progressCancelButton) {
                dispose();
                if (JDialogStandalonePlugin.isExitRequired()) {
                    System.gc();
                    System.exit(0);
                }
            }
        }

        public FormStructure getFullFormStructure() {
            return fullFormStructure;
        }
    }

    
    /**
     * Pops up the Data dictionary page. If cdePage is null go to default DDT page: "https://fitbir.nih.gov/content/data-dictionary".
     * 
     * @param cdePage The name of the CDE page.
     */
    private static void showCDE(final String cdePage) {
    	final URI ddtURI;
        final String ddtPage = "https://fitbir.nih.gov/content/data-dictionary";
        try {
            if (cdePage == null) {
            	ddtURI = new URI(ddtPage);
            } else {
            	ddtURI = new URI(cdePage);
            }
            
            Desktop.getDesktop().browse(ddtURI);
        } catch (final URISyntaxException e) {
            e.printStackTrace();
            displayError("Unable to display Data Dictionary page : " + ddtPage);
            return;
        } catch (final IOException e) {
            e.printStackTrace();
            displayError("Unable to display Data Dictionary page : " + ddtPage);
            return;
        }
    }

    
    
    /**
     * 
     * @author mcmatt
     *
     */
    public class DataElementValue {
        private GroupRepeat deGroup;

        private JLabel deLabel;
        private JComponent deComp;
        private JTextField otherSpecifyField;
        private StructuralDataElement deInfo;
        private String deValue;
        private int dePosition;

        private RequiredType deRequiredType;

        public DataElementValue(final GroupRepeat group, final JLabel label, final JComponent comp, final MapElement info, final String val) {
            deGroup = group;
            deLabel = label;
            setComp(comp);
            deInfo = info.getStructuralDataElement();
            dePosition = info.getPosition();
            deRequiredType = info.getRequiredType();
            deValue = val;
        }

        public DataElementValue(final GroupRepeat group, final MapElement info) {
            deGroup = group;
            deInfo = info.getStructuralDataElement();
            dePosition = info.getPosition();
            deRequiredType = info.getRequiredType();
        }

        public String getGroupName() {
            return deGroup.getGroupInfo().getName();
        }

        public GroupRepeat getGroup() {
            return deGroup;
        }

        public void setGroup(final GroupRepeat deGroup) {
            this.deGroup = deGroup;
        }

        public JLabel getLabel() {
            return deLabel;
        }

        public void setLabel(final JLabel deLabel) {
            this.deLabel = deLabel;
        }

        public JComponent getComp() {
            return deComp;
        }

        public void setComp(final JComponent deComp) {
            this.deComp = deComp;

           /** if (isLegacyOtherSpecifyField(this)) {
                otherSpecifyField = new JTextField();
                otherSpecifyField.setVisible(false);
                ((JComboBox) deComp).addActionListener(new OtherSpecifyListener(otherSpecifyField));
            }*/
        }

        public JTextField getOtherSpecifyField() {
            return otherSpecifyField;
        }

        public void setOtherSpecifyField(final JTextField tf) {
            otherSpecifyField = tf;
        }

        public String getName() {
            return deInfo.getName();
        }

        public StructuralDataElement getDataElementInfo() {
            return deInfo;
        }

        public void setDataElementInfo(final StructuralDataElement deInfo) {
            this.deInfo = deInfo;
        }

        public int getPosition() {
            return dePosition;
        }

        public void setPosition(final int dePosition) {
            this.dePosition = dePosition;
        }

        public RequiredType getRequiredType() {
            return deRequiredType;
        }

        public void setRequiredType(final RequiredType deRequiredType) {
            this.deRequiredType = deRequiredType;
        }

        public String getValue() {
            return deValue;
        }

        public void setValue(final String deValue) {
            this.deValue = deValue;
        }
    }
    
    /**
     * 
     * @author mcmatt
     *
     */

    public class GroupRepeat {
        private RepeatableGroup groupInfo;

        private FormStructureData parentStruct;

        private Vector<DataElementValue> dataElements;

        private int repeatNumber;

        public GroupRepeat(final RepeatableGroup groupInfo, final FormStructureData parentStruct, final Vector<DataElementValue> dataElements,
                final int repeatNumber) {
            this.groupInfo = groupInfo;
            this.parentStruct = parentStruct;
            this.dataElements = dataElements;
            this.repeatNumber = repeatNumber;
        }

        public GroupRepeat(final RepeatableGroup groupInfo, final FormStructureData parentStruct, final int repeatNumber) {
            this.groupInfo = groupInfo;
            this.parentStruct = parentStruct;
            this.repeatNumber = repeatNumber;
            this.dataElements = new Vector<DataElementValue>();
        }

        public RepeatableGroup getGroupInfo() {
            return groupInfo;
        }

        public void setGroupInfo(final RepeatableGroup groupInfo) {
            this.groupInfo = groupInfo;
        }

        public FormStructureData getParentStruct() {
            return parentStruct;
        }

        public void setParentStruct(final FormStructureData parentStruct) {
            this.parentStruct = parentStruct;
        }

        public void addDataElement(final DataElementValue de) {
            dataElements.add(de);
        }

        public Vector<DataElementValue> getDataElements() {
            return dataElements;
        }

        public void setDataElements(final Vector<DataElementValue> dataElements) {
            this.dataElements = dataElements;
        }

        public int getRepeatNumber() {
            return repeatNumber;
        }

        public void setRepeatNumber(final int repeatNumber) {
            this.repeatNumber = repeatNumber;
        }
    }

    /**
     * 
     * @author mcmatt
     *
     */
    private class FormStructureData {
        
    	private FormStructure structInfo;
    	private final Hashtable<String, Vector<GroupRepeat>> groupTable;

    	
        public FormStructureData(final FormStructure structInfo) {
            this.structInfo = structInfo;
            groupTable = new Hashtable<String, Vector<GroupRepeat>>();
        }

        public void addGroup(final String groupName) {
            groupTable.put(groupName, new Vector<GroupRepeat>());
        }

        public void addGroupRepeat(final String groupName, final GroupRepeat repeat) {
            repeat.setRepeatNumber(groupTable.get(groupName).size());
            groupTable.get(groupName).add(repeat);
        }

        public void removeLastGroupRepeat(final String groupName) {
            final int numRepeats = groupTable.get(groupName).size();
            if (numRepeats > 0) {
                groupTable.get(groupName).removeElementAt(numRepeats - 1);
            }
        }

        public Vector<GroupRepeat> getAllGroupRepeats(final String groupName) {
            return groupTable.get(groupName);
        }

        public int getNumGroupRepeats(final String groupName) {
            return groupTable.get(groupName).size();
        }

        public GroupRepeat getCurrentGroupRepeat(final String groupName) {
            return groupTable.get(groupName).lastElement();
        }

        public boolean isGroupRepeatSet(final String groupName, final int groupNum) {
            return groupNum < groupTable.get(groupName).size();
        }

        public GroupRepeat getGroupRepeat(final String groupName, final int groupNum) {
            return groupTable.get(groupName).get(groupNum);
        }

        public FormStructure getStructInfo() {
            return structInfo;
        }

        public void setStructInfo(final FormStructure structInfo) {
            this.structInfo = structInfo;
        }

        public DataElement getDataElement(final StructuralDataElement deInfo) {
            return getStructInfo().getDataElements().get(deInfo.getNameAndVersion());
        }

        public boolean isDataElementInForm(final String groupName, final String deName) {
            return (structInfo.getRepeatableGroupByName(groupName) != null)
                    && (structInfo.getRepeatableGroupByName(groupName).getMapElementByName(deName) != null);
        }
    }

    
    /** The default size that all buttons should be. */
    private static final Dimension defaultButtonSize = new Dimension(90, 30);
    
    /** The default size that all buttons should be. */
    private static final Dimension defaultTextFieldSize = new Dimension(350, 30);
    
    /**
     * Sets the location of the window to the center of the screen.
     * 
     * @param window Window that is to be displayed
     */
    private static void centerOnScreen(final Window window) {
        final Toolkit toolkit = Toolkit.getDefaultToolkit();

        final Dimension screenSize = toolkit.getScreenSize();

        int x_location = 0;
        int y_location = 0;

        if (screenSize.getSize().width > window.getSize().width) {
            x_location = (screenSize.width / 2) - (window.getSize().width / 2);
        }

        if (screenSize.getSize().height > window.getSize().height) {
            y_location = (screenSize.height / 2) - (window.getSize().height / 2);
        }

        window.setLocation(x_location, y_location);
    }
    
    
    /**
     * Sets the location of the window to the center of the parent window.
     * 
     * @param parentWindow the window where the child will be centered on.
     * @param childWindow the window that is to be displayed centered on the parent window
     */
    private static void centerInWindow(final Window parentWindow, final Window childWindow) {
        final Point parentTopLeftPoint = parentWindow.getLocationOnScreen();
        final Dimension parentSize = parentWindow.getSize();

        childWindow.setLocation( (parentTopLeftPoint.x + (parentSize.width / 2)) - (childWindow.getSize().width / 2),
                (parentTopLeftPoint.y + (parentSize.height / 2)) - (childWindow.getSize().height / 2));
    }
    
    /**
     * Pops up a message dialog to display an error.
     * 
     * <p>
     * Use when an operation has failed, preventing some operation critical for the Mapper to continue running normally or an
     * operation can neither be completed nor its errors accommodated.
     * </p>
     * 
     * @param error the message text of the error
     * 
     */
    private static void displayError(final String error) {
        if ( !GraphicsEnvironment.isHeadless()) {
    
            try {
                JOptionPane.showMessageDialog(null, error, "Error", JOptionPane.ERROR_MESSAGE);
            } catch (final Exception ex) {
                System.err.println("Exception ocurred: <" + ex.getMessage() + ">.  \n");
            }
            
        } else {
            System.err.println("Error: " + error);
            System.exit(1);
        }
        
    }
    
    /**
     * Pops up a message dialog to display a warning.
     * 
     * <p>
     * Use when an operation has failed, but the can be completed but the output may display inaccurately; i.e., errors
     * can be accommodated.
     * </p>
     * 
     * @param warning the message text of the warning.
     */
    private static void displayWarning(final String warning) {
        if ( !GraphicsEnvironment.isHeadless()) {
            
                try {
                    JOptionPane.showMessageDialog(null, warning, "Warning", JOptionPane.WARNING_MESSAGE);
                } catch (final Exception ex) {
                    System.err.println("Exception ocurred: <" + ex.getMessage() + ">.  \n");
                }
            
        } else {
            System.err.println("Warning: " + warning);
        }
    }
    
    /**
     * Helper method to build a text button.
     *
     * @param   text      Text for button.
     * @param   toolTip   Tool tip to be associated with button.
     * @param   action    Action command for button.
     * @param   listener  the listener for this button's actions
     *
     * @return  a new text button
     */
    private static final JButton buildTextButton(String text, String toolTip, String action, ActionListener listener) {
        JButton button = new JButton(text);
        button.addActionListener(listener);
        button.setToolTipText(toolTip);
        button.setFont(new Font("Serif", Font.BOLD, 12));
        button.setMinimumSize(new Dimension(20, 20));
        button.setPreferredSize(new Dimension(90, 20));
        button.setMargin(new Insets(2, 7, 2, 7));
        button.setActionCommand(action);

        return button;
    }
    
    /**
     * Builds a titled border with the given title, an etched border, and the proper font and color.
     *
     * @param   title  Title of the border
     *
     * @return  The titled border.
     */
    private static TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, new Font("Serif", Font.BOLD, 12),
                                Color.black);
    }
    
    /**
     * Create a new scroll pane, containing a component.
     *
     * @param   component        the component to put inside the scroll pane
     *
     * @return  the new scroll pane
     */
    private static final JScrollPane buildScrollPane(JComponent component) {
        JScrollPane scrollPane = new JScrollPane(component, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                 JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        return scrollPane;
    }
    
    private static final ScrollTextArea buildScrollTextArea(Color bg) {
    	ScrollTextArea tArea = new ScrollTextArea(bg);
    	return tArea;
    }
    
    /**
     * ScrollPane with an accessible JTextArea
     *
     */
    public static final class ScrollTextArea extends JScrollPane {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 3869765356771292936L;

        /** accessible JTextArea */
        private JTextArea tArea = null;

        /**
         * Creates a new ScrollTextArea object.
         */
        public ScrollTextArea(Color text_background) {
            super(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            //getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
            tArea = new JTextArea();
            tArea.setBackground(text_background);
            tArea.setEditable(true);
            tArea.setFont(new Font("Serif", Font.PLAIN, 12));
            tArea.setMargin(new Insets(3, 3, 3, 3));
            setViewportView(tArea);
        }

        /**
         * JTextArea accessor
         *
         * @return contained JTextArea
         */
        public JTextArea getTextArea() {
            return tArea;
        }
    }
    
    
    /**
     * This is a simple class that creates a DefaultTableModel with uneditable cells. With the cells uneditable, the table
     * may react to mouse events, such as double clicking on a row.
     *
     */

    public class LocalTableModel extends DefaultTableModel implements Serializable {

        //~ Static fields/initializers -------------------------------------------------------------------------------------

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 487484434950231158L;

        //~ Constructors ---------------------------------------------------------------------------------------------------

        /**
         * Calls super constructor.
         */
        public LocalTableModel() {
            super();
        }

        /**
         * Constructs a new table model with the given column names and row count.
         *
         * @param  columnNames  Names of columns.
         * @param  rowCount     Number of rows.
         */
        public LocalTableModel(Object[] columnNames, int rowCount) {
            super(columnNames, rowCount);
        }

        //~ Methods --------------------------------------------------------------------------------------------------------

        /**
         * Returns the class of the item in the column.
         *
         * @param   col  Column index.
         *
         * @return  Class of column.
         */
        public Class<? extends Object> getColumnClass(int col) {

            try {
                Vector v = (Vector) dataVector.elementAt(0);

                return v.elementAt(col).getClass();
            } catch (NullPointerException e) {
                return super.getColumnClass(col);
            }
        }

        /**
         * Gets the index of the given column name.
         *
         * @param   name  Name of column.
         *
         * @return  Index of column when created, or -1 if none.
         */
        public int getColumnIndex(String name) {

            for (int i = 0; i < columnIdentifiers.size(); i++) {

                if (name.equals(columnIdentifiers.elementAt(i))) {
                    return i;
                }
            }

            return -1;
        }

        /**
         * Gets the index of a column name that starts with the given string
         * @param name String name of column
         * @return int Index of column, or -1 if none
         */
        public int getColumnStartsWithIndex(String name) {

            for (int i = 0; i < columnIdentifiers.size(); i++) {

                if (((String)columnIdentifiers.elementAt(i)).startsWith(name)) {
                    return i;
                }
            }
            return -1;
        }

        /**
         * Gets the index of the given column name.
         *
         * @param   name  Name of column.
         *
         * @return  Index of column when created, or -1 if none.
         */
        public int getColumnBaseIndex(String name) {
            int strLength;
            for (int i = 0; i < columnIdentifiers.size(); i++) {
                strLength = ((String)columnIdentifiers.elementAt(i)).length();
                if ((strLength >= name.length()) &&
                    (name.equals(((String)columnIdentifiers.elementAt(i)).substring(0,name.length())))) {
                    return i;
                }
            }

            return -1;
        }

        /**
         * Returns false for all cells, so none can be edited.
         *
         * @param   x  x value of cell
         * @param   y  y value of cell
         *
         * @return  false, always
         */
        public boolean isCellEditable(int x, int y) {
            return false;
        }

        /**
         * Sets the value at the given row and column of the table.
         *
         * @param  value  Value to set.
         * @param  row    Row to set value at.
         * @param  col    Column to set value at.
         */
        public void setValueAt(Object value, int row, int col) {
            super.setValueAt(value, row, col);
            fireTableCellUpdated(row, col);
        }

    }
    
    
    
    /**
     * This is a simple class that creates a DefaultTableModel with editable cells. With the cells editable, the table
     * may react to mouse events, such as double clicking on a row.
     *
     */

    public class LocalMappingTableModel extends DefaultTableModel  {

        //~ Static fields/initializers -------------------------------------------------------------------------------------

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 487484434950231158L;

        //~ Constructors ---------------------------------------------------------------------------------------------------

        /**
         * Calls super constructor.
         */
        public LocalMappingTableModel() {
            super();
        }
        
        /**
         * Gets the index of the given column name.
         *
         * @param   name  Name of column.
         *
         * @return  Index of column when created, or -1 if none.
         */
        public int getColumnIndex(String name) {

            for (int i = 0; i < columnIdentifiers.size(); i++) {

                if (name.equals(columnIdentifiers.elementAt(i))) {
                    return i;
                }
            }

            return -1;
        }

        /**
         * Some cells can be edited
         *
         * @param   x  x value of cell
         * @param   y  y value of cell
         *
         * @return  
         */
        public boolean isCellEditable(int x, int y) {
            //if (y > deTableModel.getColumnIndex("Required") && y < deTableModel.getColumnIndex("Source PVs")) return true;
            //else return false;
        	return false;
        }

       
    }


}
