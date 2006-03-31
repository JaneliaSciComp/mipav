package gov.nih.mipav.view;

import java.awt.*;
import javax.swing.*;

/**
 * This class was created for use in the JDialogMultiPaint class. It
 * is intended to show a border around a button to indicate that it
 * is 'selected' or 'highlighted'.
 * 
 * @author orsinol
 *
 */
public class BorderedButton extends JButton 
{
	private boolean borderOn = false;
	private Color borderColor = Color.yellow;
	
	public BorderedButton(String text)
	{
		super(text);
	}
	
	public void paintComponent(Graphics graphics)
	{
		super.paintComponent(graphics);

		if (borderOn)
		{
			graphics.setColor(borderColor);
			graphics.drawRect(2, 2, getSize().width - 5, getSize().height - 5);
			graphics.drawRect(3, 3, getSize().width - 7, getSize().height - 7);
		}
	}
	
	public void setBorderOn(boolean state)
	{
		borderOn = state;
		
		repaint();
	}
	
	public boolean isBorderOn()
	{
		return borderOn;
	}
	
	public void setBorderColor(Color newBorderColor)
	{
		borderColor = newBorderColor;
		
		repaint();
	}
	
	public Color getBorderColor()
	{
		return borderColor;
	}
	
	public void setSelected(boolean selected)
	{
		super.setSelected(selected);
		
		setBorderOn(selected);
	}

}
