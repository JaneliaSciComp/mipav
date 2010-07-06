/**
 * 
 */
package gov.nih.mipav.view;

import gov.nih.mipav.model.file.FileDicomItem;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;

/**
 * @author morseaj
 *
 */
public class JTreeDICOM extends JTree {


	private static final long serialVersionUID = 1L;

	public JTreeDICOM(DefaultMutableTreeNode base) {
		super(base);
	}

	public String convertValueToText(Object value, boolean selected,
	        boolean expanded, boolean leaf, int row,
	        boolean hasFocus) 
	{
		FileDicomItem current;
		DefaultMutableTreeNode currentNode;
		if(value != null) {
			currentNode = (DefaultMutableTreeNode)value;
			
			if(!currentNode.toString().equalsIgnoreCase("DICOMDIR"))
			{
				current = (FileDicomItem) currentNode.getUserObject();
	    	    String currentItemType = current.getTag("0004,1430").getValue(true).toString(); 
	    	    if (currentItemType.startsWith("PATIENT"))
	    	    {
	    	    	return "Patient- " +current.getTag("0010,0020").getValue(true).toString();
	    	    }
	    	    else if(currentItemType.startsWith("STUDY"))
	    	    {
	    	    	return "Study " +current.getTag("0020,0010").getValue(true).toString().trim() +
	    	    	" (UID-" + current.getTag("0020,000D").getValue(true).toString().trim()+")";
	    	    }
	    	    else if (currentItemType.startsWith("SERIES"))
	    	    {
	    	    	return "Series " +current.getTag("0020,0011").getValue(true).toString().trim() +
	    	    	" (UID-" + current.getTag("0020,000E").getValue(true).toString().trim()+")";
	    	    }
	    	    else if(currentItemType.startsWith("IMAGE"))
	    	    {
	    	    	return "Image " +current.getTag("0020,0013").getValue(true).toString() +
	    	    	" (" + current.getTag("0004,1500").getValue(true).toString().trim()+")";
	    	    }
	    	    return "";
			} 
			else
				return "DICOMDIR";
		}
		return "";
	}
	
}