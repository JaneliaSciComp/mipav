import gov.nih.mipav.model.structures.event.VOIEvent;
import gov.nih.mipav.model.structures.event.VOIListener;
import gov.nih.mipav.view.ColorIcon;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.JButton;

/**
 * The ColorButton inside of a ColorButtonPanel
 */
class PlugInMuscleColorButton extends JButton implements VOIListener {

	/**The icon displaying the current color.*/
	private ColorIcon cIcon;
	
	/**The name of the voi corresponding to this button.*/
	private String voiName;
	
	/**Maintains reference to containing panel for easy action processing*/
	private PlugInMuscleColorButtonPanel container;
	
	/**
	 * Constructs a colorButton with the given color and name 
	 * with standard dimension of 20px by 20px.
	 */
	public PlugInMuscleColorButton(Color c, String voiName, PlugInMuscleColorButtonPanel container) {
		super();
		cIcon = new ColorIcon(c, 13, 13);
		this.voiName = voiName;
		this.container = container;
		
		setIcon(cIcon);
		setForeground(ColorIcon.TRANSPARENT);
		setBackground(ColorIcon.TRANSPARENT);
		setBorder(null); 
		setSize(new Dimension(20,20));
		setPreferredSize(new Dimension(20,20));
	}
	
	public PlugInMuscleColorButtonPanel getContainer() {
		return container;
	}
	
	/**
     * We are not interested in adding curves, so this method is empty.
     */
	public void addedCurve(VOIEvent added) {
        /* not interested in adding curves */
	}
	
    /**
	 * VOI Listener call (listens only to color changes)
	 */
	public void colorChanged(Color c) {
		cIcon.setColor(c);
		this.repaint();
	}

	/**
	 * Returns the colorIcon inside of the colorButton.
	 */
	public ColorIcon getColorIcon() {
		return cIcon;
	}
	
	/**
	 * Returns the name of the VOI corresponding to this colorButton.
	 */
	public String getVOIName() {
		return voiName;
	}

	/**
	 * We are not interested in selecting Curves, so this method is empty.
	 */
	public void selectedVOI(VOIEvent selection) {
		/* not interested in having the ColorButon select VOIs */
	}

	/**
     * We are not interested in removing Curves, so this method is empty.
     */
    public void removedCurve(VOIEvent removed) {
        /* not interested in removing curves */
    }
}