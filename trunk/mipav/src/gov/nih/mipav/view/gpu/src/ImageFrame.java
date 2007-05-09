package gov.nih.mipav.view.gpu.src;

import java.awt.*;
/**
* written by Sieuwert van Otterloo
* Ernst van Rheenen
* Utrecht 2001 www.bluering.nl
* goodguys@bluering.nl
*/

public class ImageFrame extends Frame
{
 public static int opened=0;
 static int count=0;

 ImageFrame(ImageCanvas a)
 {
  super("ImageFrame "+(count++));
  resize(a.sx+30,a.sy+30);
  setResizable(false);
  setLayout(new BorderLayout());
  add("Center",a);
  opened++;
  show();
 }

 /**
 * if autoexit is set the java program is exited
 * if the last imageframe is closed.
 */
 public static boolean autoexit=true;



 public boolean handleEvent(Event ev)
 {/*If the close window button (right, marked 'x')
  is pressed, the program is terminated, otherwise
  the event is handled the same as before.*/
  if (ev.id==Event.WINDOW_DESTROY)
  {
	hide();
	opened--;
	try{finalize();}catch(Throwable e){};
	if(opened==0&&autoexit)
		System.exit(0);
	return true;
  }
  return super.handleEvent(ev);
 }

}
