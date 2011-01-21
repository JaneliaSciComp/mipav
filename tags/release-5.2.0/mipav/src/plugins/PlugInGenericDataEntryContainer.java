import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.view.*;

import info.clearthought.layout.*;

import java.awt.Component;
import java.awt.Dimension;
import java.util.List;

import javax.swing.*;

/**
 * Container class to hold tabs for each data structure 
 * @author jhunter
 * @params a list of Data Structure short names you wish to add data to
 */
public class PlugInGenericDataEntryContainer extends JDialogStandalonePlugin {
	private static final long serialVersionUID = 5480334277478582203L;
	
	private static JTabbedPane pane = new JTabbedPane();
	
	public PlugInGenericDataEntryContainer(final List<String> selections){
		final double[][] frameSize = {{TableLayout.FILL},{TableLayout.FILL}};
		setLayout(new TableLayout(frameSize));
		
//		String targetDataStruct = (String)JOptionPane.showInputDialog(new PlugInDataStructureSelection(), "Please Select A Data Structure to Edit"
//				, "Choose A Data Structure", JOptionPane.QUESTION_MESSAGE, null, null, null);
		
		PlugInGenericDataEntryContainer.pane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
		addPane(selections);
		
		add(PlugInGenericDataEntryContainer.pane, "0,0");
		
		Icon icon = null;
        try {
        	icon = new ImageIcon(MipavUtil.getIconImage(PlugInDialogDataEntryTool.ICON_NAME));
            setIconImage(MipavUtil.getIconImage(PlugInDialogDataEntryTool.ICON_NAME));
        } catch (final Exception e) {
        }
		setTitle("Genomics and Clinical Assessments Data Entry Tool");
		setSize(new Dimension(1070,500));
		setName("Data Entry Tool");
        setVisible(true);
	}
	
	/**
	 * Used when loading a new tab from a file
	 * @param name - data structure info list with name and version
	 * @param cols - list of columns
	 * @param entries - data entry rows
	 */
	public static void addPane(final List<String> name, final List<String> cols, final List<PlugInGenomicsEntry> entries){
		PlugInGenericDataEntryContainer.pane.addTab(name.get(0).toUpperCase()+name.get(1).toUpperCase()
		+" (From File)", new PlugInDataInputTable(name,cols,entries));
		PlugInGenericDataEntryContainer.pane.setSelectedIndex(
				PlugInGenericDataEntryContainer.pane.getTabCount()-1);
	}
	
	/**
	 * 
	 * @param selections - data structures selected from table to populate tabbed pane
	 */
	public static void addPane(final List<String> selections){
		for(final String sel : selections)
			PlugInGenericDataEntryContainer.pane.add(sel.toUpperCase(), new PlugInDataInputTable(sel));
		PlugInGenericDataEntryContainer.pane.setSelectedIndex(
				PlugInGenericDataEntryContainer.pane.getTabCount()-1);
	}
	
	public static void removePane(Component c){
		PlugInGenericDataEntryContainer.pane.remove(c);
		if(PlugInGenericDataEntryContainer.pane.getTabCount()==0)
			new PlugInDialogDataEntryTool(true);
	}
	
	
}
