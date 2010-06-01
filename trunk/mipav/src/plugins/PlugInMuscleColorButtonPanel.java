import gov.nih.mipav.model.structures.event.VOIEvent;
import gov.nih.mipav.model.structures.event.VOIListener;
import gov.nih.mipav.view.ColorIcon;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

/**
 * simple class that contains a ColorIcon for displaying the VOI color, and 
 * has a button for changing the color of the linked VOI (linked by name)
 * @author linkb
 *
 */
public class PlugInMuscleColorButtonPanel extends JPanel {

	private PlugInMuscleColorButton colorButton;
	private ActionListener container;
	
	public PlugInMuscleColorButtonPanel(Color c, String voiName, ActionListener container) {
		super();
		this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		this.container = container;
		colorButton = new PlugInMuscleColorButton(c, voiName, this);
		colorButton.addActionListener(container);
		add(colorButton);
	}
	
	/**
	 * Returns the actual colorButton inside of the panel.
	 */
	public PlugInMuscleColorButton getColorButton() {
		return colorButton;
	}
	
	/**
	 * Returns the listener to this panel
	 */
	public ActionListener getContainer() {
		return container;
	}

	
	
	/**
	 * Whether thecolorButton is visible.
	 */
	public boolean isSelected() {
		if(colorButton.getColorIcon().getColor() == Color.BLACK) 
			return false;
		return true;
	}

	/**
	 * Sets the color of this colorButton.
	 */
	public void setColor(Color c) {
		colorButton.getColorIcon().setColor(c);
	}
}
	
	