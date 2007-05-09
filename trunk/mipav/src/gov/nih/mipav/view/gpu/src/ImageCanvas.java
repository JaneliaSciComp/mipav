package gov.nih.mipav.view.gpu.src;
import java.io.*;
import java.awt.*;
import java.awt.image.*;

public class ImageCanvas extends Canvas
/*This class can show an image object*/
{
 public int sx,sy;
 private BufferedImage bufferim;
 private Image24 im;


 protected ImageCanvas(Image24 im)
 {
  this.im=im;
  sx=im.sx;
  sy=im.sy;
  bufferim = new BufferedImage(sx,sy,BufferedImage.TYPE_4BYTE_ABGR);
  super.resize(sx,sy);
  download();
  /*the size of this canvas is the same as the size
  of the bitmap that is shown.*/
  setBackground(Color.black);
 }

 /**
 * updates the Image24 object with the changes made on the
 * canvas
 */
 private Graphics g;

 /**
 * get a Graphics object for doing paint operations.
 */
 public Graphics getGr()
 {
  if(g==null)
  	g=bufferim.createGraphics();
  return g;
 }
 /**
 * get an image. needed for storejpg
 */
 public Image getawtImage()
 {return bufferim;
 }

/**
* upload the information in bufferim to the Image24
*/
 void upload()
 {
  bufferim.getRGB(0,0,sx,sy,im.data,0,sx);
 }
 /**
 * update the canvas with changes on the im24;
 */
 void download()
 {
  bufferim.setRGB(0,0,sx,sy,im.data,0,sx);
  repaint();
 }

 /*by redefining update I suppress flickering. */
 public void update(Graphics g)
    {paint(g);}


 public void paint(Graphics real){
  im.trydownload();
  resize(sx,sy);
  real.drawImage(bufferim,0,0,this);/*draw the bufferimage on screen*/
 }

 /**
 * create a Frame and show this image hierin.
 */
 public void show()
 {Frame f=new ImageFrame(this);
 }

 /**
 * test and examples
 */
 public static void fractaldemo()
 {
  Image24 out=Image24.fractaljulia(0.40,-0.11,200,200,-1.5,-1.5,2,2);
  out.linear(14,0,20,0,8,0,1,0);
  out.storejpg(new File("d:/test/imagetoolz/julia.jpg"));
  out.getIC().show();
  out=Image24.fractalmandelbrot(200,200,-1,0.245,0.1,0.1);
  out.storejpg(new File("d:/test/imagetoolz/mandelb.jpg"));
  out.linearmod(4,0,1,0,1,0,1,0);
  out.storejpg(new File("d:/test/imagetoolz/flinmod.jpg"));
  out.getIC().show();
 }

 public static void transformdemo()
 {
  Image24 out;
  Image24 im=Image24.loadjpg(new java.io.File("sieuwert.jpg"));
  im.getIC().show();
  out=new Image24(400,400);
  //out.rotate(im,im.sx/2.0,im.sy/2.0,200,200,0);
  //out.getIC().show();
  out=im.newrotatedcopy(Math.PI/3,2);
  out.getIC().show();
 }
 public static void winddemo()
 {
  Image24 out;
  Image24 im=Image24.loadjpg(new java.io.File("sieuwert.jpg"));
  out= new Image24(im);
  out.convolute(im.newblurfilter(21,1),false);
  out.getIC().show();
  out= new Image24(im);
  out.convolute(im.newblurfilter(1,21),false);
  out.getIC().show();
  out= new Image24(im);
  out.blur(5,5);
  out.getIC().show();
  im.getIC().show();
 }


 public static void replacedemo()
 {
  Image24 out;
  Image24 im=Image24.loadjpg(new java.io.File("sieuwert.jpg"));
  im.getIC().show();
  out=new Image24(im);
  int x=24;
  out.colorreplace(239-x,239+x,238-x,238+x,243-x,243+x,255,256,im.color(255,192,255,255));
  out.getIC().show();
  im=new Image24(150,100);
  im.replacegradient(0,10,10,100,80,im.color(200,200,255,255),im.color(200,255,200,255));
  im.getIC().show();
 }
 public static void webdemo()
 {
 Image24 out;
 Image24 fade;
 Image24 im=Image24.loadjpg(new java.io.File("sieuwert.jpg"));
 im.getIC().show();
 if (im!=null)
 {
  fade=new Image24(im);
  fade.fill(0);
  fade.replacegradient(0,0,0,im.sx-1,im.sy-1,im.color(255,220,240,255),im.color(0,30,0,255));
  fade.storejpg(new java.io.File("fade.jpg"));
  out=new Image24(im);
  out.gamma(0.8,1.5,1.2,1);
  out.storejpg(new java.io.File("gamma.jpg"));
  out=new Image24(im);
  out.invert(true,true,false,false);
  out.storejpg(new java.io.File("invert.jpg"));
  out=new Image24(im);
  out.blur(3,3);
  out.storejpg(new java.io.File("blur.jpg"));
  out=new Image24(im);
  out.scatter(20,10);
  out.storejpg(new java.io.File("scatter.jpg"));
  out=new Image24(im);
  out.pseudocolor();
  out.storejpg(new java.io.File("pseudo.jpg"));
  out=im.newrotatedcopy(45,0.7);
  out.storejpg(new java.io.File("rotate.jpg"));
  out=new Image24(im);
  out.translate(out.sx/2,out.sy/2);
  out.storejpg(new java.io.File("translate.jpg"));
  out=new Image24(im);
  Image24 out2=new Image24(im);
  Image24 boring=new Image24(im);
  boring.fill(im.color(127,127,127,255));
  out2.rot180();
  out2.storejpg(new java.io.File("rot180.jpg"));
  out.comparemax(out2,out,out,out2);
  out.storejpg(new java.io.File("comparemax.jpg"));
  out=new Image24(im);
  Image24 text=Fancytext.shadowtext(
   new Font("Courier",Font.PLAIN,40),
   "blueRing",
   im.color(0,200,0,255),im.color(0,0,0,150),14);
  text=text.scale(out.sx,(text.sy*out.sx)/text.sx);
  out.pasteover(text,0,0);
  out.storejpg(new java.io.File("shadowtext.jpg"));
  out=new Image24(im);
  out.linearmod(2,0,1,0,4,0,1,0);
  out.storejpg(new java.io.File("linmod.jpg"));
  out=new Image24(im);
  out.threshold(im.color(25,25,150,255),im.color(200,200,255,255),140);
  out.storejpg(new java.io.File("threshold.jpg"));
  out=new Image24(im);
  int[] palette={out.color(0,0,255,255),out.color(100,0,100,255),
  	out.color(200,0,30,255),out.color(255,150,150,255)};
  out.band(palette);
  out.storejpg(new java.io.File("band.jpg"));
  out=new Image24(im);
  out.addimage(fade,1,1,2);
  out.storejpg(new java.io.File("addimage.jpg"));
  out.getIC().show();
  }
 }
 public static void speeddemo()
 {
  Image24 im=Image24.loadjpg(new java.io.File("sieuwert.jpg"));
  im.fill(10,10,30,20,im.color(200,255,200,255));
  long t1=System.currentTimeMillis();
  im.storeppm(new java.io.File("speedtest.ppm"));
  long t2=System.currentTimeMillis();
  System.out.println("time="+(t2-t1));
 }


 public static void channeldemo(boolean save)
 {
  Image24 im=Image24.loadjpg(new java.io.File("sieuwert.jpg"));
  Image24 out;
  Image24[] channels=
  {new Image24(im),new Image24(im),new Image24(im),new Image24(im),new Image24(im),new Image24(im),new Image24(im)};
  channels[0].red();
  channels[1].green();
  channels[2].blue();
  channels[3].alpha();
  channels[4].HSB(0);
  channels[5].HSB(1);
  channels[6].HSB(2);
  for(int i=0;i<channels.length;i++)
   channels[i].getIC().show();
  out=new Image24(channels[0]);
  out.combineHSL(channels[4],channels[5],channels[6]);
  out.getIC().show();
 }

 public static void linmoddemo()
 {
  Image24 im=new Image24(100,100);
  im.replacegradient(0,0,0,100,100,im.color(0,0,0,255),im.color(255,255,255,255));
  //im.linearmod(1,0,4,0,8,0,1,0);
  im.getIC().show();


 }

 public static void textdemo()
 {
  Image24 bg=Image24.loadjpg(new java.io.File("sieuwert.jpg"));
  Image24 im=null;
  im=Fancytext.rainbowtext(new Font("Gill Sans",0,40),"Rainbow");
  im.getIC().show();
  bg.pasteover(im,bg.sx/2-im.sx/2,bg.sy-im.sy);
  bg.getIC().show();
 }


 public static void main(String[] ps)
 {
   boolean save=false;
   //webdemo();
   //speeddemo();
   //transformdemo();
   //replacedemo();
   //winddemo();
   //fractaldemo();
   //linmoddemo();
   textdemo();
   System.out.println("finished");
 }


}
