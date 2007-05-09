package gov.nih.mipav.view.gpu.src;
import java.util.*;
import java.io.File;
import java.awt.*;
import java.awt.image.*;
import java.math.*;
import java.io.*;


public class Image24{
 /************ data members *********************************/
 int sx; //width
 int sy; //height
 int[] data; //the image data: one int per pixel
 /**
 * the ImageCanvas holds a java.awt.Image with the same
 * content. This allows one to do the standard java operations
 * like Stringdrwaing on a Image24.
 */
 ImageCanvas myic;
 /**
 * if download is set the Image24 has changed and these changes
 * have not been done on the ImageCanvas
 * (the IC must 'download' these changes)
 */
 boolean download;
 /**
 * if upload is set the IC has changed and these changes
 * have not been done on the Image24
 * (the IC must 'upload' these changes)
 */
 boolean upload;



 /*********** constructors ****************************/
 /**
 * construct a new Image24 with the indicated size. It is
 * fully transparent when created.
 */
 public Image24(int x,int y){
  sx=x;
  sy=y;
  data=new int[sx*sy];
  for(int i=0;i<data.length;i++)
   data[i]=0;
 }

 /**
 * create a copy of the Image24
 */
 public Image24(Image24 im){
   im.tryupload();
   sx=im.sx;
   sy=im.sy;
   data=new int[sx*sy];
   for(int i=0;i<data.length;i++)
    data[i]=im.data[i];
 }

 /****************** updownloading ***********/

 /**
 * call this method before doing an operation
 * on the Image24. It sets download
 */
 void startoperation()
 {tryupload();
  download=true;
 }

 /**
 * does an upload if necessary
 * Call this method if you want to read the
 * data array directly.
 */
 void tryupload()
 {if (upload)
  { getIC().upload();
    upload=false;
  }
 }
 /**
 * does a download if necessary
 */
 void trydownload()
 {if (download)
  {
    download=false;
    getIC().download();
  }
 }


 /**
 * synchronises the double buffering. If you did
 * operations on one of the buffers, it will copy
 * the operations to the other. call update if you want
 * to see the changes on the Image24 on screen.
 */
 public void update()
 {
  tryupload();
  trydownload();
 }

 /**
 * get the ImageCanvas belong to this image.
 * since an ImageCanvas is a Canvas you can add it to
 * a component to see the image.
 */
 public ImageCanvas getIC()
 {
  if (myic==null)
  {
  	upload=download=false;
	myic=new ImageCanvas(this);
  }
  trydownload();
  return myic;
 }
 /**
 * get the graphics object. Using this oject you can
 * apply all standard java image operations on an Image24
 * Do not store the result
 * of this method, because if you do that the Image24 cannot
 * manage the up/downloading automatically.
 */
 public Graphics getGr()
 {
  upload=true;
  return getIC().getGr();
 }
 /*************** image basics **************/

 /**
 * returns true if this and the given image have the same size.
 */
 public boolean samesize(Image24 im)
 {return im.sx==sx&&im.sy==sy;
 }
 /*getwidth and getheight give the size of the Image24 in pixels*/
 public int getwidth(){return sx;}
 /*getwidth and getheight give the size of the Image24 in pixels*/
 public int getheight(){return sy;}

/* get the index into the data array for pixel x,y*/
 int index(int x,int y){
  return sx*y+x;
 }

 /**
 * set the pixel at position (x,y) with value v
 */
 public void set(int x,int y,int v)
 {
  startoperation();
  data[index(x,y)]=v;
 }
 /**
 * get the value of the pixel at (x,y). If x and y lie
 * outside the image, the closest boundary value is returned.
 */
 public int extget(int x,int y)
 {
  tryupload();
  if(y<0) y=0;
  if(x<0) x=0;
  if(y>=sy) y=sy-1;
  if(x>=sx) x=sx-1;
  return data[index(x,y)];
 }

 /**
 *  get the value of the pixel at (x,y). If x and y lie
 * outside the image, the background value bg is returned.
 */
 public int bgget(int x,int y,int bg)
 {
  tryupload();
  if(y<0) return bg;
  if(x<0) return bg;
  if(y>=sy) return bg;
  if(x>=sx) return bg;
  int r=data[index(x,y)];
  return r;
 }


 /**
 * get the value of the pixel at (x,y). It is required
 * that (x,y) lies inside the image. If not, a wrong value
 * is returned or an ArrayIndexOutOfBoundsException is thrown.
 */
 public int get(int x,int y)
 {
  tryupload();
  return data[index(x,y)];
 }
/**
* get a pixel using linear interpolation of the 4 surrounding pixels
* w and v must be arrays of length 4. They are given so they can be reused
* bg is the background color that is returned for pixels outside the image.
*/
 public int linget(double x, double y,double[] w,int[] v,int bg)
 {
  int flx=(int)Math.floor(x);
  int clx=flx+1;
  int fly=(int)Math.floor(y);
  int cly=fly+1;
  //System.out.println(x+":"+y);
  v[0]=bgget(flx,fly,bg);
  w[0]=(clx-x)*(cly-y);
  v[1]=bgget(flx,cly,bg);
  w[1]=(clx-x)*(y-fly);
  v[2]=bgget(clx,fly,bg);
  w[2]=(x-flx)*(cly-y);
  v[3]=bgget(clx,cly,bg);
  w[3]=(x-flx)*(y-fly);
  //System.out.println("w "+w[0]+":"+w[1]+":"+w[2]+":"+w[3]);
  //System.out.println("v "+v[0]+":"+v[1]+":"+v[2]+":"+v[3]);
  return mix(v,w);
  //return color(100,200,20,255);
 }

/********** color channel operations ***************************/

  /**
 * create a pixel value
 * @param r the amount of red. 0<=r<256.
 * @param g the amount of green. 0<=g<256.
 * @param b the amount of blue. 0<=b<256.
 * @param a the amount of opacity. 0<=a<256. 0 is
 * completely transparent, 255 completely opaque.
 */
 public static int color(int r,int g,int b,int alpha){
  int result=(alpha<<24)+(r<<16)+(g<<8)+b;
  return result;
 }
 /**
 * convert a pixel value to a java Color object.
 */
 public static Color c2C(int c)
 {return new Color(c,true);}
 /**
 * convert a java Color object to a pixel value.
 */
 public static int C2c(Color c)
 {return color(c.getRed(),c.getGreen(),c.getBlue(),c.getAlpha());
 }

 /**
 * return a shade of gray
 * @param v the brightness. 0<=v<256.
 */
 public static int gray(int v,int alpha)
 {return (alpha<<24)+(v<<16)+(v<<8)+v;
 }

 private final static int CALPHA=0,CRED=1,CGREEN=2,CBLUE=3;
 private static int[] cshift={24,16,8,0};
 private static int[] cmask={~((1<<24)-1),(1<<24)-(1<<16),(1<<16)-256,255};


 /**
 * the amount of red in the pixelvalue.
 * @return a value between of 0, 255 or inbetween
 */
 public static int red(int v)
 {return (v&cmask[CRED])>>>cshift[CRED];}
 /**
 * the amount of green in the pixelvalue.
 * @return a value between of 0, 255 or inbetween
 */
 public static int green(int v)
 {return (v&cmask[CGREEN])>>>cshift[CGREEN];}
 /**
 * the amount of blue in the pixelvalue.
 * @return a value between of 0, 255 or inbetween
 */
 public static int blue(int v)
 {return (v&cmask[CBLUE])>>>cshift[CBLUE];}

 /**
 * the amount of transparency in the pixelvalue.
 * @return a value between of 0, 255 or inbetween
 */
 public static int alpha(int v)
 {return (v&cmask[CALPHA])>>>cshift[CALPHA];}


 /**
 * return the pixelvalue for the indicated color
 * @param r the amount of red: 0(no red) to 1(full red)
 * @param g the amount of green: 0(no green) to 1(full green)
 * @param b the amount of red: 0(no red) to 1(full blue)
 * @param a the amount of alpha:
 *	0(completely transparent) to 1(fully opaque)
 */
 public static int dcolor(double r,double g,double b,double a)
 {return color(range(0,256,r*255),range(0,256,g*255),
 			   range(0,256,b*255),range(0,256,a*255));
 }


 /** Set the image with the gray value corresponding
 * to its hue, saturation of brightness
 * @param c c=0 will give hue, c=1 saturation and c=2 brightness
 */
 public void HSB(int c)
 {
  startoperation();
  float[] fa1=new float[3];
  for(int i=0;i<data.length;i++)
  {
  	int v=data[i];
 fa1=Color.RGBtoHSB(red(v),green(v),blue(v),fa1);
    data[i]=gray(range(0,256,256*fa1[c]),alpha(v));
  }
 }

 /** Set the red channel with the average value from
 * the given source. */
 public void copyred(Image24 src)
 {setchannel(src,cshift[CRED],cmask[CRED]);}
 /** Set the green channel with the average value from
 * the given source. */
 public void copygreen(Image24 src)
 {setchannel(src,cshift[CGREEN],cmask[CGREEN]);}
 /** Set the blue channel with the average value from
 * the given source. */
 public void copyblue(Image24 src)
 {setchannel(src,cshift[CBLUE],cmask[CBLUE]);}
 /** Set the alpha channel with the average value from
 * the given source. */
 public void copyalpha(Image24 src)
 {setchannel(src,cshift[CALPHA],cmask[CALPHA]);}

 /** Set the selected channel with the average value from
 * the given source. channels are red,green,blue and alpha
 */
 void setchannel(Image24 src,int shift,int mask)
 {
  src.tryupload();
  startoperation();
  if (!samesize(src))
  {
  	System.out.println("setchannel error: source is not the same size");
    return;
  }
  int nmask=~mask;
  for(int i=0;i<data.length;i++)
  {
   data[i]= (data[i]&nmask)|(avg(src.data[i])<<shift);
  }
 }
 /**
 * set the Hue, Sat or Brightness according to the average of the
 * source.
 */
 public void setHSBchannel(Image24 src,int c)
 {
  src.tryupload();
  startoperation();
  if (!samesize(src))
  { System.out.println("setchannel error: source is not the same size");
    return;
  }
  float[] fsrc=new float[3];
  float[] ftgt=new float[3];
  int s,t;
  for(int i=0;i<data.length;i++)
  {
   t=data[i];
   s=src.data[i];
   Color.RGBtoHSB(red(s),green(s),blue(s),fsrc);
   ftgt[c]=(float)(avg(s)/256.0);
   data[i]=Color.HSBtoRGB(ftgt[0],ftgt[1],ftgt[2]);
  }
 }

 public void combineHSL(Image24 hue,Image24 sat, Image24 bright)
 {
  hue.tryupload();
  sat.tryupload();
  bright.tryupload();
  startoperation();
  if (!samesize(hue)||!samesize(sat)||!samesize(bright))
  { System.out.println("setchannel error: source is not the same size");
    return;
  }
  for(int i=0;i<data.length;i++)
  {
   int h,s,b;
   h=avg(hue.data[i]);
   s=avg(sat.data[i]);
   b=avg(bright.data[i]);
   data[i]=Color.HSBtoRGB((float)(h/256.0),(float)(s/256.0),(float)(b/256.0));
  }
 }

 /**
 * return the average of the red, green and blue in the pixel
 * @param r the relative importance of red
 * @param g the relative importance of green
 * @param b the relative importance of blue
 * @param s the sum of r,g,b.
 */
 public static int avg(int v,int r,int g,int b,int s)
 {
  return (r*red(v)+
  		 g*green(v)+
 	 b*blue(v))/s;
 }

 /**
 * standard brightness
 */
 public static int avg(int v)
 {return avg(v,299,587,114,1000);
 }

 private int[] newcolorband(double start, double speed,
 	double satstart,double satspeed, double bstart,double bspeed)
 {
  int[] r=new int[256];
  for(int i=0;i<r.length;i++)
  {
   double hue=start+speed*(i/256.0);
   double sat=satstart+satspeed*(i/256.0);
   double bright=bstart+bspeed*(i/256.0);
   r[i]=Color.getHSBColor((float)hue,(float)sat,(float)bright).getRGB();
  }
  return r;
 }

 public void pseudocolor()
 {
  band(newcolorband(0,1,1,0,0.8,0));
 }


 /**
 * splits the image according to avg
 */
 void band(int[] colors)
 {
  startoperation();
  int l=colors.length;
  for(int i=0;i<data.length;i++)
  	data[i]=colors[(avg(data[i])*l)/256];
 }

 /**
 * set each pixel with the graylevel of its
 * brightness
 */
 public void gray()
 {avg(299,587,114,1000);
 }
 /**
 * set each pixel with the graylevel of its
 * red value
 */
 public void red()
 {avg(1,0,0,1);
 }
 /**
 * set each pixel with the graylevel of its
 * green value
 */
 public void green()
 {avg(0,1,0,1);
 }
 /**
 * set each pixel with the graylevel of its
 * blue value
 */
 public void blue()
 {avg(0,0,1,1);
 }
 /**
 * set each pixel with the graylevel of its
 * alpha value
 */
 public void alpha()
 {
  startoperation();
  for(int i=0;i<data.length;i++)
  	data[i]=gray(alpha(data[i]),255);
 }


 void avg(int r,int g,int b,int s)
 {
  startoperation();
  for(int i=0;i<data.length;i++)
  	data[i]=gray(avg(data[i],r,g,b,s),alpha(data[i]));
 }

 /***************** pixelwise operations **************/

 /**
 * split the image in a high and low part
 * @param color of the low part
 * @param color of the high part
 * @param minlight the minimal average for a
 * pixel to become high.
 */
 public void threshold(int vdark,int vlight,int minlight)
 {
  startoperation();
  int a;
  for(int i=0;i<data.length;i++)
  {
 	a=avg(data[i]);
 if(a<minlight)
 	data[i]=vdark;
 else
 	data[i]=vlight;
  }
 }

 /**
 * Invert the colors of the image. invert(true,true,true,false) is the
 * most common invert operation.
 * @param flipr make it true if you want red inverted
 * @param flipg make it true if you want green inverted
 * @param flipb make it true if you want blue inverted
 * @param flipa make it true if you want alpha inverted
 */
 void invert(boolean flipr,boolean flipg,boolean flipb,boolean flipa)
 {
  startoperation();
  int v,r,g,b,a;
  for(int i=0;i<data.length;i++)
  {
 	v=data[i];
    r=red(v);
    r=flipr?255-r:r;
    g=green(v);
    g=flipg?255-g:g;
    b=blue(v);
    b=flipb?255-b:b;
    a=alpha(v);
    a=flipa?255-a:a;
    data[i]=color(r,g,b,a);
  }
 }

 private static int range(int min,int max,double dv)
 {
  int v=(int)Math.round(dv);
  if(v<min)
  	return min;
  return (v>=max)?max-1:(int)v;
 }

 private static int rangemod(int max,double dv)
 {
  int v=((int)Math.round(dv))%max;
  if(v<0)
  	 v+=max;
  return v;
 }

 /**
 * adjust brightness of each channel. Adds the parameter
 * to the channel.
 */
 public void brightness(int difr,int difg,int difb,int difa)
 {linear(1,difr,1,difg,1,difb,1,difa);
 }

 /**
 * do gamma correction on each channel separately
 */
 public void gamma(double gr,double gg,double gb,double ga)
 {
  startoperation();
  int v,r,g,b,a;
  gr=1/gr;gg=1/gg;gb=1/gb;ga=1/ga;

  for(int i=0;i<data.length;i++)
  {
 	v=data[i];
    r=range(0,256, 255*Math.pow((red(v)/255.0),gr) );
    g=range(0,256, 255*Math.pow((green(v)/255.0),gg) );
    b=range(0,256, 255*Math.pow((blue(v)/255.0),gb) );
    a=range(0,256, 255*Math.pow((alpha(v)/255.0),ga) );
    data[i]=color(r,g,b,a);
  }
 }
 /**
 * adjust the contrast for each channel.
 * @param sr adjustment parameter for the red channel. A
 * value greater than 1 will increase red difference, a value
 * below 1 will weaken the red differences.
 * @param sg adjustment parameter for the green channel.
 * works the same as sr.
 * @param sb adjustment parameter for the blue channel.
 * works the same as sr.
 * @param sa adjustment parameter for the alpha channel.
 * works the same as sr.
 */
 void contrast(double sr,double sg,double sb,double sa)
 {linear(sr,128-128*sr,sg,128-128*sg,sb,128-128*sb,sa,128-128*sa);
 }

 /**
 * do a linear transformation on each channel of each pixel
 * according to the formula newred=oldred*sr+dr. Similar
 * for green, blue and alpha.
 */
 public void linear(double sr,double dr,double sg,double dg,
 			 double sb,double db,double sa, double da)
 {
  startoperation();
  int v,r,g,b,a;
  for(int i=0;i<data.length;i++)
  {
 	v=data[i];
    r=range(0,256,red(v)*sr+dr);
    g=range(0,256,green(v)*sg+dg);
    b=range(0,256,blue(v)*sb+db);
    a=range(0,256,alpha(v)*sa+da);
    data[i]=color(r,g,b,a);
  }
 }

 /**
 * do a linear transformation on each channel of each pixel
 * according to the formula newred=oldred*sr+dr. If the
 * outcome exceeds the possible values, it is wrapped around:
 * -1 becomes 255, and 260 becomes 4.
 */
 public void linearmod(double sr,double dr,double sg,double dg,
 			 double sb,double db,double sa, double da)
 {
  startoperation();
  int v,r,g,b,a;
  for(int i=0;i<data.length;i++)
  {
 	v=data[i];
    r=rangemod(256,red(v)*sr+dr);
    g=rangemod(256,green(v)*sg+dg);
    b=rangemod(256,blue(v)*sb+db);
    a=rangemod(256,alpha(v)*sa+da);
    data[i]=color(r,g,b,a);
  }
 }


 /*********** filtering **********************/

 /**
 * blur the image: smoothen transitions and remove edges.
 * @param size the size of the filter
 * @param times the number of times filtering must take place
 */
 public void blur(int size,int times)
 {
   for(int i=0;i<times;i++)
    convolute(newblurfilter(size,size),i%2==0);
 }


 public void wind(int hblur,int vblur)
 {
  convolute(newblurfilter(hblur,vblur),true);
 }

 /**
 * create a filter for blurring an image.
 * a larger filter gives more blur.
 * @param sx the horizontal size
 * @param sy the vertical size
 */
 public double[][] newblurfilter(int sx,int sy)
 {
  double[][] result=new double[sx][sy];
  for(int i=0;i<sx;i++)
  	for(int j=0;j<sy;j++)
 	result[i][j]=1.0/(sx*sy);
  return result;
 }

 /**
 * apply a linear filter to the image.
 * @param d a square matrix of coefficients. The numbers
 * in it can be positive or negative, and preferrably add up
 * to 1.
 */
 public void convolute(double[][] d, boolean evenodd)
 {
  startoperation();
  int[] newdata=new int[sx*sy];
  int halfsizex=d.length/2;
  int halfsizey=d[0].length/2;
  int filterstartx=-halfsizex;
  int filterstarty=-halfsizey;
  int filterendx=halfsizex;
  int filterendy=halfsizey;
  if(d.length%2==0)
  	if (evenodd)
 {filterstartx++;filterstarty++;}
 else
 {filterendx--;	filterendy--;}
  double salpha,sred,sblue,sgreen;
  int v;
  double weight;
  for(int x=0;x<sx;x++)
  	for(int y=0;y<sy;y++)
  	{
     salpha=sred=sgreen=sblue=0;
  	 for(int i=filterstartx;i<=filterendx;i++)
      for(int j=filterstarty;j<=filterendy;j++)
      {v=extget(x+i,y+j);
       weight=(alpha(v)/255.0)*d[i-filterstartx][j-filterstarty];
       salpha+=weight;
       sred+=(red(v)/255.0)*weight;
       sgreen+=(green(v)/255.0)*weight;
       sblue+=(blue(v)/255.0)*weight;
      }
  if(salpha<=0)
  	newdata[index(x,y)]=color(0,0,0,0);
  else
  	newdata[index(x,y)]=
 	 dcolor(sred/salpha,sgreen/salpha,sblue/salpha,salpha);
 }
  data=newdata;
 }
 /**
 * mix the pixels with the given relative weights;
 */
 static int mix(int[] pixel,double weight[])
 {
  double salpha=0,sred=0,sblue=0,sgreen=0,w,alphaw;
  int v;
  for(int i=0;i<weight.length;i++)
  {
   v=pixel[i];
   w=weight[i];
   alphaw=w*alpha(v)/255.0;
   salpha+=alphaw;//lies in 0 - 1
   sred+=red(v)*alphaw/255.0;//sred lies in 0 - 1
   sgreen+=green(v)*alphaw/255.0;
   sblue+=blue(v)*alphaw/255.0;
  }
  if(salpha<=0)
  	 return color(0,0,0,0);
  return dcolor(sred/salpha,sgreen/salpha,sblue/salpha,salpha);
 }

 /**
 * apply a distortion that moves pixels around
 */
 void scatter(int hsize,int vsize)
 {
  Random ran=new Random(
    System.currentTimeMillis()+(long)hsize*vsize+(long)sx*sy);
  startoperation();
  int[] newdata=new int[sx*sy];
  for(int j=0;j<sy;j++)
  for(int i=0;i<sx;i++)
   newdata[index(i,j)]=
   	extget(i+ran.nextInt(hsize)-hsize/2,j+ran.nextInt(vsize)-vsize/2);
  data=newdata;
 }


 /************ affine transformations ****************/

/**
* returns a scaled version of the current image
* @param newx the new width
* @param newy the new height
*/
 public Image24 scale(int newx, int newy)
 {
  Image24 result=new Image24(newx,newy);
  result.startoperation();
  double x2,y2;
  double[] weight=new double[4];
  int[] val=new int[4];
  int bg=color(0,0,0,0);
  for(int j=0;j<newy;j++)
  {
   y2=(j*sy*1.0)/newy;
   for(int i=0;i<newx;i++)
   {
    x2=(i*sx*1.0)/newx;
    result.set(i,j,linget(x2,y2,weight,val,bg));
   }
  }
  return result;
 }

 /** rotate/translate src nto this image (previous content is overwritten)
 *  according to the matrix
 * x    x1	x2	tx			srcx
 * y  =	y1	y2	ty	* 		srcy
 * 1	0	0	1			1
 * it sets every pixel of this Image with the rotated
 * contents of src, or with the value background
 */
 public void transform(Image24 src,double x1,double x2, double tx,
 	double y1, double y2,double ty,int background)
 {
  startoperation();
  src.tryupload();
  double a,b,c,d,det,btx,bty;
  //invert the given matrix
  det=x1*y2-y1*x2;
  bty=(tx*y1-ty*x1)/det;
  btx=(ty*x2-tx*y2)/det;
  if (det==0)
   {a=d=1;b=c=0;tx=ty=100000;}
  else
  {a=y2/det;
   b=-x2/det;
   c=-y1/det;
   d=x1/det;
  }
  double[] storage1=new double[4];
  int[] storage2=new int[4];
  for(int j=0;j<sy;j++)
   for(int i=0;i<sx;i++)
    set(i,j,src.linget(i*a+j*b+btx,i*c+j*d+bty,storage1,storage2,background));
 }

/**
* rotate src into this Image24. This Image24 is completely overwritten.
* @param x horiz. coordinate of the center of rotation of src
* @param y vertical coordinate of the center of rotation of src
* @param cxt horiz. coordinate of the new position of the center
* of rotation in this Image24
* @param cyt vertical coordinate of the new position of the center
* of rotation in this Image24
* @param deg rotation angle in degrees. positive means counterclockwise
* @param scale magnification factor of the image
*/

 public void rotate(Image24 src,double x,double y,double cxt,double cyt,double deg,double scale)
 {
  double sin=scale*Math.sin(Math.toRadians(deg));
  double cos=scale*Math.cos(Math.toRadians(deg));
  //cxt*=scale;cyt*=scale;
  transform(src,cos,sin,-cos*x-y*sin+cxt,-sin,cos,x*sin-cos*y+cyt,color(0,0,0,0));
 }

 /**
 * return a rotated copy of this Image24.
 * @param deg rotation angle in degrees. positive means counterclockwise
 * @param scale magnification factor of the image
 */
 public Image24 newrotatedcopy(double deg,double scale)
 {
  double sin=scale*Math.abs(Math.sin(Math.toRadians(deg)));
  double cos=scale*Math.abs(Math.cos(Math.toRadians(deg)));
  int nsx=(int)Math.ceil(cos*sx+sy*sin);
  int nsy=(int)Math.ceil(sin*sx+sy*cos);
  Image24 result=new Image24(nsx,nsy);
  result.rotate(this,sx/2.0,sy/2.0,nsx/2.0,nsy/2.0,deg,scale);
  return result;
 }
 /**
 * slide/shift/translate the image. The image in wrapped around:
 * anything shifted out will shift in at a different position.
 */
 public void translate(int dx,int dy)
 {
  startoperation();
  int[] newdata=new int[data.length];
  dx=rangemod(sx,dx);
  dy=rangemod(sy,dy);
  for(int j=0;j<sy;j++)
  	for(int i=0;i<sx;i++)
	 newdata[index((i+dx)%sx,(j+dy)%sy)]=get(i,j);
  data=newdata;
 }
 public void rot180()
 {
  startoperation();
  int dummy;
  for(int i=0;i<data.length/2;i++)
  {
   dummy=data[i];
   data[i]=data[data.length-1-i];
   data[data.length-1-i]=dummy;
  }
 }
 /*************** multi-image operations ***********/


 /**
 * add src2 channelwise to this image. w1/div is the relative weight
 * of this image, w2/div the relative weight of w2. Choose div=w1+w2
 * if you want to take an average.
 * This operation is completely channelwise. Alpha has no special meaning
 */

 public void addimage(Image24 src2,int w1,int w2,int div)
 {
  if(!samesize(src2))
  	return;
  startoperation();
  src2.tryupload();
  int v,v2,r,g,b,a;
  for(int i=0;i<data.length;i++)
  {
 	v=data[i];
    v2=src2.data[i];
	r=range(0,256,(red(v)*w1+red(v2)*w2)/div);
    g=range(0,256,(green(v)*w1+green(v2)*w2)/div);
    b=range(0,256,(blue(v)*w1+blue(v2)*w2)/div);
    a=range(0,256,(alpha(v)*w1+alpha(v2)*w2)/div);
    data[i]=color(r,g,b,a);
  }
 }

 /**
 * place im1 over this picture (thus using transparency)
 */
 public void pasteover(Image24 src,int fx,int fy)
 {
  int srcx1=0,srcx2;
  int srcy1=0,srcy2;
  srcx1=(fx>=0)?0:-fx;
  srcy1=(fy>=0)?0:-fy;
  srcx2=(sx-fx<src.sx)?sx-fx:src.sx;
  srcy2=(sy-fy<src.sy)?sy-fy:src.sy;
  for(int j=srcy1; j<srcy2; j++)
  {
   for(int i=srcx1; i<srcx2; i++)
    set(i+fx,j+fy,mergepixel(src.get(i,j),get(i+fx,j+fy)));
  }
 }
  /**
 * place im1 in this picture ignoring the previous values
 */
 public void pasteinto(Image24 src,int fx,int fy)
 {
  int srcx1=0,srcx2;
  int srcy1=0,srcy2;
  srcx1=(fx>=0)?0:-fx;
  srcy1=(fy>=0)?0:-fy;
  srcx2=(sx-fx<src.sx)?sx-fx:src.sx;
  srcy2=(sy-fy<src.sy)?sy-fy:src.sy;
  System.out.println("paste"+src.sx+" in "+sx+" at "+fx+" w="+srcx2);
  for(int j=srcy1; j<srcy2; j++)
  {
   for(int i=srcx1; i<srcx2; i++)
    set(i+fx,j+fy,src.get(i,j));
  }
 }

 /**
 * This function fills this with pixelvalues from src1 and src2.
 * All 4 parameters and this must be equalsized. It is allowed that
 * the parameters equal each other or this.
 * A pixel gets the value from src1 if cp1>=cp2, src2 otherwise.
 */

 public void comparemax(Image24 cp1,Image24 cp2,Image24 src1,Image24 src2)
 {
  if(!samesize(cp1)||!samesize(cp2)||!samesize(src1)||!samesize(src2))
   return;
  startoperation();
  cp1.tryupload();
  cp2.tryupload();
  src1.tryupload();
  src2.tryupload();
  for(int i=0;i<data.length;i++)
   data[i]=cp1.data[i]>=cp2.data[i]?src1.data[i]:src2.data[i];
 }



 /************** basic painting *****************/

 /**
 * fill the entire image with the given value.
 */
 public void fill(int v)
 {fill(0,0,sx,sy,v);
 }

 /**
 * fill a rectangle with the given value
 * @param x horizontal coordinate of upper left corner
 * @param y vertical coordinate of upper left corner
 * @param w width of the rectangle
 * @param h height of the rectangle
 * @param v the pixelvalue
 */
 public void fill(int x,int y,int w,int h,int v){
  startoperation();
  if(x+w>sx) 	w=sx-x;
  if(y+h>sy)    h=sy-y;
  for(int j=0;j<h;j++){
   int start=index(x,y+j);
   int end=index(x+w,y+j);
   for(;start<end;start++)
    data[start]=v;
  }
 }


 /**
 * alignment constants for use in drawString
 * VERYDOWN means no character will stick out.
 */
 public final static int UPRIGHT=1,CENTER=0,DOWNLEFT=-1,VERYDOWN=-2;
/**
* draw a string in this image. It uses java.awt methods
* (so it calls getGr()).
* @param s the text to draw
* @param x the horizontal position
* @param y the vertical position
* @param halign the horizontal alignment. If it is UPRIGHT the
* text appears on the right of the position, if CENTER it appears
* on both sides of x,y and if DOWNLEFT it appears left of x,y
* @param valign the horizontal alignment. If it is UPRIGHT the
* text appears above the x,y position, if CENTER it appears
* on both sides of x,y and if DOWNLEFT it appears below x,y
*/
 public void drawString(String s,int x,int y,int halign,int valign)
 {
  FontMetrics fm=getGr().getFontMetrics();
  int w=fm.stringWidth(s);
  fm.getMaxAscent();
  if(halign==UPRIGHT)x-=w;
  else if(halign==CENTER)x-=w/2;
  if(valign==UPRIGHT)
   y+=fm.getMaxAscent();
  else if(valign==CENTER)
   y+=fm.getAscent()/3;//3=wet finger work. looks best
  else if(valign==VERYDOWN)
   y-=fm.getMaxDescent();
  getGr().drawString(s,x,y);
 }

 public static int stringwidth(String s,Font f)
 {return Toolkit.getDefaultToolkit().getFontMetrics(f).stringWidth(s);
 }

 public static int stringheight(String s,Font f)
 {FontMetrics fm=Toolkit.getDefaultToolkit().getFontMetrics(f);
  return fm.getAscent()+fm.getDescent();
 }

 Image24 makeshadow(int color)
 {
  Image24 alevel=new Image24(this);
  int alpha=alpha(color);
  alevel.alpha();
  Image24 result=new Image24(this.sx,this.sy);
  result.colorreplace(0,color);
  result.copyalpha(alevel);
  if(alpha!=255)
   result.linear(1,0,1,0,1,0,alpha/255.0,0);
  return result;
 }



 public final static int cBlack=color(0,0,0,255);
 public final static int cWhite=color(255,255,255,255);
 public final static int cRed=color(255,0,0,255);
 public final static int cGreen=color(0,255,0,255);
 public final static int cBlue=color(0,0,255,255);
 public final static int cGlass=color(0,0,0,0);
 public final static int cSmoke=color(128,128,128,128);





 /**
 * returns a black text in the current color
 * against a transparent background.
 */
 public static Image24 rendertext(Font f,String s,int leftm,int rightm,int topm, int downm)
 {
  int x=stringwidth(s,f);
  int y=stringheight(s,f);
  Image24 r=new Image24(leftm+x+rightm,topm+y+downm);
  r.getGr().setColor(Color.black);
  r.getGr().setFont(f);
  r.drawString(s,leftm,topm,r.DOWNLEFT,r.UPRIGHT);
  return r;
 }

 /****************** color replace operators ***********/

 /**
 * replace the oldv pixelvalue with the newv pixelvalue
 */
 public void colorreplace(int oldv,int newv)
 {
  startoperation();
  for(int i=0;i<data.length;i++)
  	if(data[i]==oldv)
		data[i]=newv;
 }
 /**
 * give all pixels that fall in the given boundaries the newv value
 * Pixels are replaced if rmin<=red<rmax, gmin<=green<gmax,etc.
 */
 public void colorreplace(int rmin,int rmax,int gmin,
 		int gmax,int bmin, int bmax, int amin, int amax,int newv)
 {
  startoperation();
  int r,g,b,a;
  for(int i=0;i<data.length;i++)
  {
  	r=red(data[i]);
	if(r<rmin||r>=rmax)
		continue;
	r=green(data[i]);
	if(r<gmin||r>=gmax)
		continue;
	r=blue(data[i]);
	if(r<bmin||r>=bmax)
		continue;
	r=alpha(data[i]);
	if(r<amin||r>=amax)
		continue;
	data[i]=newv;
  }
 }

 public void colorfloodfill(
   int x,int y,int tolerance,int newv,int level)
 {
  tryupload();
  int v=get(x,y);
  colorfloodfill(red(v)-tolerance,red(v)+tolerance+1,
   green(v)-tolerance,green(v)+tolerance+1,
   blue(v)-tolerance,blue(v)+tolerance+1,
   alpha(v)-tolerance,alpha(v)+tolerance+1,
   newv,x,y,level);
 }
 /**
 * replace the colors with all channels
 * inside the intervals with newv, flooding from x,y
 * with level as maximal path.
 */
 public void colorfloodfill(int rmin,int rmax,
 		int gmin,int gmax,
		int bmin, int bmax,
		int amin, int amax,
		int newv,int x,int y,int level)
 {
  int hypercolor=color(101,102,103,0);//unused color
  colorreplace(hypercolor,0);//remove old values
  startoperation();
  colorfloodfill2(rmin,rmax,gmin,gmax,bmin,bmax,amin,amax,
  	hypercolor,x,y,level);
  colorreplace(hypercolor,newv);
 }

 private void colorfloodfill2(
    int rmin,int rmax,int gmin,int gmax,
 	int bmin, int bmax, int amin, int amax,
	int newv,int x,int y,int level)
 {
  int v=get(x,y);
  if(v==newv||red(v)<rmin||red(v)>=rmax||green(v)<gmin||green(v)>=gmax)
 	 return;
  if(blue(v)<bmin||blue(v)>=bmax||alpha(v)<amin||alpha(v)>=amax)
 	 return;
  set(x,y,newv);
  if (level>0)
  {
   if(x>0)
    colorfloodfill2(rmin,rmax,gmin,gmax,bmin,bmax,amin,amax,
					newv,x-1,y,level-1);
   if(y>0)
    colorfloodfill2(rmin,rmax,gmin,gmax,bmin,bmax,amin,amax,
  				newv,x,y-1,level-1);
   if(x+1<sx)
    colorfloodfill2(rmin,rmax,gmin,gmax,bmin,bmax,amin,amax,
  				newv,x+1,y,level-1);
   if(y+1<sy)
    colorfloodfill2(rmin,rmax,gmin,gmax,bmin,bmax,amin,amax,
  			newv,x,y+1,level-1);
  }
 }



 /**
 * replace a color by the given image. The image is repeated if it is too small
 */
 public void colorreplace(int oldv,Image24 im2)
 {
  startoperation();
  im2.tryupload();
  for(int j=0;j<sy;j++)
  	for(int i=0;i<sx;i++)
  	 if(get(i,j)==oldv)
	 	set(i,j,im2.get(i%im2.sx,j%im2.sy));
 }

 //calculate an inbetween color.
 private int colorgrad(double value, int low, int high,int[] values,double[] w)
 {
  w[0]=(high-value)/(high-low);
  w[1]=(value-low)/(high-low);
  return mix(values,w);
 }

 /**
 * replace oldv with a gradient color: x1,y1 will have color c1,
 * x2,y2 will have color c2, and inbetween pixel will get a color inbetween.
 */
 public void replacegradient(int oldv,int x1,int y1,int x2,int y2, int c1,int c2)
 {
  startoperation();
  int dx=x2-x1;
  int dy=y2-y1;
  int low=x1*dx+y1*dy;
  int high=x2*dx+y2*dy;
  int[] values={c1,c2};
  double[] weight=new double[2];
  for(int j=0;j<sy;j++)
   for(int i=0;i<sx;i++)
 	if (get(i,j)==oldv)
	 set(i,j,colorgrad(i*dx+j*dx,low,high,values,weight));
 }

 /**
 * return the value that results when placing p1
 * over p2. If alpha(p1)=0 it returns p2, if alpha(p1)=1
 * it returns p1.
 */
 static int mergepixel(int p1, int p2)
 {int c1,c2,r,g,b,m1,m2,m3,
      a1,a2,a3;
  a1 = alpha(p1);
  a2 = alpha(p2);
/** the colors are mixed using the formula
* a1*r1+((1-a1)*a2)*r2 / (a1+(1-a1)*a2)
* with a1 and a2 the alpha levels in the range 0--1
*/
  m1 = 255*a1;
  m2 = (255-a1)*a2;
  m3 = (255*a1+(255-a1)*a2);
  if (m3==0)
  	{m1=1;m2=1;m3=2;}//if completely transparent, mix equally
  r = (m1*red(p1)+m2*red(p2))/m3;
  g = (m1*green(p1)+m2*green(p2))/m3;
  b = (m1*blue(p1)+m2*blue(p2))/m3;
  a3 = 255 - ((255-a1)*(255-a2))/255;
  return color(r,g,b,a3);
 }



 /*************** loading and saving images ********************/

 /* utilities for filereading and writing*/
 private static void writeintLSB(FileOutputStream raf,int v)
 throws IOException
 {
  raf.write(v&255);v=v>>8;
  raf.write(v&255);v=v>>8;
  raf.write(v&255);v=v>>8;
  raf.write(v&255);v=v>>8;
 }
 private static void writeshortLSB(FileOutputStream raf,int v)
 throws IOException
 {
  raf.write(v&255);v=v>>8;
  raf.write(v&255);v=v>>8;
 }
 private static int b2i(int v)
 {return v<0?v+256:v;
 }

 private static int readintLSB(RandomAccessFile raf)
 throws IOException
 {
  int v=b2i(raf.readByte());
  v+=b2i(raf.readByte())<<8;
  v+=b2i(raf.readByte())<<16;
  v+=b2i(raf.readByte())<<24;
  return v;
 }
 private static int readshortLSB(RandomAccessFile raf)
 throws IOException
 {
  int v=b2i(raf.readByte());
  v+=b2i(raf.readByte())<<8;
  return v;
 }

 //get a ascii integer for ppm file.
 private static int getnum(RandomAccessFile raf)
 throws IOException
 {
  String s="";
  int c=raf.readByte();
  while (c<'0'||c>'9')
  {
   if(c=='#')
   	while(c!='\n')
 	 c=raf.readByte();
   c=raf.readByte();
  }
  while (c>='0'&&c<='9')
  { s+=(char)c;
    c=raf.readByte();
  }
  return Integer.parseInt(s);
 }

 /**
 * store the image in file f in Windows Bitmap (bmp) format
 */
 public void storebmp(File f)
 {
  tryupload();
  try
  {
   if(f.exists())
   		f.delete();
   FileOutputStream raf=new FileOutputStream(f);
   raf.write('B');raf.write('M');
   int linelength=sx*3;
   while(linelength%4!=0)
   	linelength++;
   writeintLSB(raf,sy*linelength+54); //size of file in bytes
   writeintLSB(raf,0); //reserved for future use
   writeintLSB(raf,14+40); //size of global+bitmap header
   writeintLSB(raf,40); //bitmap header size
   writeintLSB(raf,sx); //image width in pixels
   writeintLSB(raf,sy); //height
   writeshortLSB(raf,1); //# of color planes
   writeshortLSB(raf,24); //bits per pixel
   writeintLSB(raf,0); //method of compression
   writeintLSB(raf,linelength*sy); //size of the image data
   writeintLSB(raf,10000); //pixels/metre horizontal
   writeintLSB(raf,10000); //pixels/metre vertical
   writeintLSB(raf,0); //# number of colors, 0 means maximal
   writeintLSB(raf,0); //# important color indices, 0 means all
   int v;
   byte[] buffer=new byte[linelength];
   int buffercursor;
   for(int j=sy-1;j>=0;j--)
   {
    buffercursor=0;
	for(int i=0;i<sx;i++)
    {
     v=data[index(i,j)];
     buffer[buffercursor++]=(byte)blue(v);
 	 buffer[buffercursor++]=(byte)green(v);
 	 buffer[buffercursor++]=(byte)red(v);
	}
    raf.write(buffer);
   }
   raf.close();
  }catch(Exception e)  {e.printStackTrace();}
 }

  /**
 * load an image stored as a Windows bitmap file
 * this method returns null in case of failure. It fails
 * if the bmp file is not a true color bmp file.
 */
 public static Image24 loadbmp(File f)
 {
  try
  {
   RandomAccessFile raf=new RandomAccessFile(f,"r");
   if (raf.readByte()!='B'||raf.readByte()!='M')
   {
	System.out.println(f+" is not a BMP file");
	return null;
   }
   int filesize=readintLSB(raf);
   if (filesize!=f.length())
   {
    System.out.println(f+"(size "+f.length()+") is not "+filesize);
    //return null;
   }
   readintLSB(raf); //reserved for future use
   readintLSB(raf); //54 size of global+bitmap header
   readintLSB(raf); //40 bitmap header size
   int sx=readintLSB(raf); //image width in pixels
   int sy=readintLSB(raf); //height
   int linelength=sx*3;
   while(linelength%4!=0)
   	linelength++;
   System.out.println(sx+":"+sy);
   if (readshortLSB(raf)!=1)
   {System.out.println(f+" does not have the right number of colorplanes");
    return null;
   }
   if (readshortLSB(raf)!=24)
   {System.out.println(f+" has a wrong number of bits per pixel");
    return null;
   }
   if (readintLSB(raf)!=0)
   {System.out.println(f+" has a wrong compression method");
    return null;
   }
   if (readintLSB(raf)!=linelength*sy)
   {System.out.println(f+" has a wrong size of image data");
    //return null;
   }
   readintLSB(raf);//pixels/metre horizontal
   readintLSB(raf);//pixels/metre vertical
   readintLSB(raf); //# number of colors, 0 means maximal
   readintLSB(raf); //# important color indices, 0 means all
   Image24 result=new Image24(sx,sy);
   int v,r,g,b;
   for(int j=sy-1;j>=0;j--)
   {
    for(int i=0;i<sx;i++)
    {
     b=raf.readByte();
	 g=raf.readByte();
	 r=raf.readByte();
	 result.data[result.index(i,j)]=result.color(r,g,b,255);
	}
	/*make sure all lines are aligned on 4byte boundaries:*/
	for(int k=sx*3;k<linelength;k++)
	  raf.readByte();
    /*the above lines insert 0-bytes to make sure a line starts
	* at a bytenumber dividable by 4.*/
   }
   raf.close();
   return result;
  }catch(Exception e)  {e.printStackTrace();}
  return null;
 }


 /**
 * load an image stored as a jpeg file
 */
 public static Image24 loadjpg(File f)
 {
  try
  {
   System.out.println(f.getPath());
  Image jpg=Toolkit.getDefaultToolkit().getImage(f.getPath());
  if(jpg==null)
  	return null;
  while (jpg.getWidth(null)==-1||jpg.getHeight(null)==-1)
   Thread.sleep(20);
  Image24 r=new Image24(jpg.getWidth(null),jpg.getHeight(null));
  while(!r.getGr().drawImage(jpg,0,0,null))
   Thread.sleep(20);
  return r;
  }catch(Exception e)
  {e.printStackTrace();
  }
  return null;
 }

 /**
 * store the image in file f in jpeg format
 */
 public void storejpg(File f){
  try
  {
   FileOutputStream dataOut = new FileOutputStream(f);
   JpegEncoder jpg = new JpegEncoder(getIC().getawtImage(), 85, dataOut);
   jpg.Compress();
   dataOut.close();
  }catch(Exception e){e.printStackTrace();}
 }

 /**
 * load an Image stored as portable pixelmap in File f.
 */
 public static Image24 loadppm(File f)
 {
  try
  {
   RandomAccessFile raf=new RandomAccessFile(f,"r");
   if (raf.readByte()!='P'||raf.readByte()!='6')
   {
    System.out.println(f+" is not a binary PPM file");
    return null;
   }
   int sx=getnum(raf);
   int sy=getnum(raf);
   int colors=getnum(raf);
   if(colors!=255)
   	System.out.println("#colors is "+colors+" but should be 255");
   int r,g,b;
   Image24 im=new Image24(sx,sy);
   for(int i=0;i<im.data.length;i++)
   {
    r=b2i(raf.readByte());
   	g=b2i(raf.readByte());
   	b=b2i(raf.readByte());
   	im.data[i]=color(r,g,b,255);
   }
   raf.close();
   return im;
  }catch(Exception e)  {e.printStackTrace();}
  return null;
 }

 /**
 * store the image in file f in portable pixmap (ppm) format
 */
 public void storeppm(File f){
  tryupload();
  try
  {
   if(f.exists())
   		f.delete();
   FileOutputStream raf=new FileOutputStream(f);
   raf.write(("P6\n#www.bluering.nl\n"+sx+" "+sy+"\n255\n").getBytes());
   byte[] buffer=new byte[3*sx];
   int c=0,d=0;
   for(int j=0;j<sy;j++)
   {
   	c=0;
    for(int i=0;i<sx;i++)
    {
	 buffer[c++]=(byte)red(data[d]);
	 buffer[c++]=(byte)green(data[d]);
	 buffer[c++]=(byte)blue(data[d++]);
    }
	raf.write(buffer);
   }
   raf.close();
  }catch(Exception e)
  {e.printStackTrace();
  }
 }

 /*********** fractals **********************/
 /**
 * draw a julia fractal with the parameter c= creal+i*cim, according
 * to the formula x<-x^2+c
 * use fractaljulia( , ,-2,-2,4,4) for the entire fractal
 * @param creal real part of the parameter c
 * @param cim imaginary part of the parameter c
 * @param sx horizontal size of the result
 * @param sy vertical size of the result
 * @param startx defines the visible part of the fractal (upper left corner)
 * @param starty defines the visible part of the fractal (upper left corner)
 * @param width the width of the visible part of the fractal
 * @param height the height of the visible part of the fractal
 * @returns a new Image 24 containing the fractal
 */

 public static Image24 fractaljulia(double creal,double cim, int sx,int sy,
  double startx,double starty,double width,double height)
 {
  Image24 result=new Image24(sx,sy);
  double[] x=new double[2];
  for(int j=0;j<sy;j++)
   for(int i=0;i<sx;i++)
   {
    x[0]=startx+(i*width)/sx;
    x[1]=starty+(j*height)/sy;
    int count;
    for(count=0;count<255;count++)
    {
     fractaliterate(x,creal,cim);
     if(fractalnorm(x)>2)
  	  break;
    }
    result.set(i,j,gray(count,255));
   }
  return result;
 }
 /**
 * draw a mandelbrot fractal
 * use fractalmandelbrot( , ,-2,-2,4,4) for the entire fractal
 * @param sx horizontal size of the result
 * @param sy vertical size of the result
 * @param startx defines the visible part of the fractal (upper left corner)
 * @param starty defines the visible part of the fractal (upper left corner)
 * @param width the width of the visible part of the fractal
 * @param height the height of the visible part of the fractal
 * @returns a new Image 24 containing the fractal
 */
 public static Image24 fractalmandelbrot(
   	int sx,int sy,
  double startx,double starty,double width,double height)
 {
  Image24 result=new Image24(sx,sy);
  double[] x=new double[2];
  double creal,cim;
  for(int j=0;j<sy;j++)
    for(int i=0;i<sx;i++)
    {
     creal=startx+(i*width)/sx;
     cim=starty+(j*height)/sy;
     x[0]=x[1]=0;
     int count;
     for(count=0;count<255;count++)
     {
      fractaliterate(x,creal,cim);
      if(fractalnorm(x)>4)
    	break;
     }
     result.set(i,j,gray(count,255));
    }
  return result;
 }


 private static void fractaliterate(double[] x,double cr,double ci)
 {
  double xold=x[0];
  double yold=x[1];
  x[0]=xold*xold-yold*yold+cr;
  x[1]=2*xold*yold+ci;
 }
 private static double fractalnorm(double[] x)
  {return x[0]*x[0]+x[1]*x[1];}



}
