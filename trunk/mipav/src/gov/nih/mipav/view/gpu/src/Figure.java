package gov.nih.mipav.view.gpu.src;

import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.image.*;
import java.math.*;

public class Figure
{
 int marginleft=60,marginright=80,margintop=40,marginbottom=40,sx,sy;
 double minx,maxx,miny,maxy;
 double xmult=1,ymult=1;
 Image24 result;

 Figure(int sx,int sy,int minx,int maxx,int miny,int maxy)
 {this.sx=sx;
  this.sy=sy;
  this.minx=minx;
  this.maxx=maxx;
  this.miny=miny;
  this.maxy=maxy;
  makeaxis();
  xmult=Math.pow(10,-3+(int)Math.floor(Math.log(maxx-minx)/Math.log(10)));
  ymult=-3+(int)Math.floor(Math.log(maxx-minx)/Math.log(10));
 }



 void setTitle(String t,String xlabel,String ylabel)
 {
  result.getGr().setColor(Color.black);
  Font oldfont=result.getGr().getFont();
  result.getGr().setFont(new Font("Arial",Font.PLAIN,30));
  result.drawString(t,result.sx/2,margintop/2,result.CENTER,result.CENTER);
  result.getGr().setFont(oldfont);
  result.drawString(xlabel,result.sx-2,result.sy-2,result.UPRIGHT,result.DOWNLEFT);
  result.drawString(ylabel,2,2,result.DOWNLEFT,result.UPRIGHT);
 }

 void setBottomline(String text)
 {
  result.drawString(text,2,result.sy-3,result.DOWNLEFT,result.DOWNLEFT);
 }

 static Color newHSBcolor(double hue,double sat,double bright,int alpha)
 {
  Color r=Color.getHSBColor((float)hue,(float)sat,(float)bright);
  return new Color(r.getRed(),r.getGreen(),r.getBlue(),alpha);
 }

 static Color[] createcolors(int n,double sat,double bright,int alpha)
 {
  Color[] result=new Color[n];
  for(int i=0;i<n;i++)
  	result[i]=newHSBcolor((double)i/n,sat,bright,alpha);
  return result;
 }

 int getx(double value)
 {return marginleft+(int)(((value-minx)*sx)/(maxx-minx));}
 int gety(double value)
 {return margintop+sy-(int)(((value-miny)*sy)/(maxy-miny));}

 static int minlabeldistx=40;
 static int minlabeldisty=30;


 private String d2s(double d,double mult)
 {
  String r=""+(int)Math.round(d*mult);
  return r;
 }
 /**
 * draw a line at the given value and label it with the name.
 */
 private void xaxismark(double v,String name)
 {result.getGr().drawLine(getx(v),gety(miny),getx(v),gety(maxy));
  result.drawString(name,getx(v),gety(miny)+2,result.CENTER,result.UPRIGHT);
 }

 private void yaxismark(double v,String name)
 {result.getGr().drawLine(getx(minx),gety(v),getx(maxx),gety(v));
  result.drawString(name,getx(minx),gety(v),result.UPRIGHT,result.CENTER);
 }

 void makeaxis()
 {
  result=new Image24(sx+marginleft+marginright,sy+margintop+marginbottom);
  result.fill(0,0,result.sx,result.sy,result.color(255,255,255,255));
  Graphics g=result.getGr();
  g.setColor(Color.gray);
  g.drawLine(getx(minx),gety(miny),getx(maxx),gety(miny));
  g.drawLine(getx(minx),gety(miny),getx(minx),gety(maxy));

  double xstep=1;//raise xstep like 1,2,5,10,20,50
  while(getx(xstep)-getx(0)>minlabeldistx)
  	xstep/=10;
  for(int i=0;getx(xstep)-getx(0)<minlabeldistx;i++)
   if(i%3==1) xstep=xstep*5/2; else xstep*=2;
  for(double i=minx-minx%xstep;i<=maxx;i+=xstep)
   xaxismark(i,d2s(i,xmult));
  g=result.getGr();
  int ystep=1;//raise xstep like 1,2,5,10,20,50
  while(Math.abs(gety(0)-gety(ystep))>minlabeldisty)
  	ystep/=10;
  for(int i=0;Math.abs(gety(0)-gety(ystep))<minlabeldisty;i++)
   if(i%3==1) ystep=ystep*5/2; else ystep*=2;
  for(double i=miny-miny%ystep;i<=maxy;i+=ystep)
    yaxismark(i,d2s(i,ymult));


  result.getGr().setColor(Color.black);
  if(Math.round(xmult)!=1.0)
  	result.drawString("x"+xmult,getx(maxx)+2,gety(miny),result.DOWNLEFT,result.CENTER);
  if(Math.round(ymult)!=1.0)
 	result.drawString("x"+ymult,getx(miny),gety(maxy)-2,result.CENTER,result.DOWNLEFT);
 }

 private static Color[] linecolors=createcolors(8,1,0.5,255);
 int lines=0;
 int pointsets=0;



 static void drawdot(Graphics g,int x,int y,int s,int dottype)
 {
  dottype=dottype%4;

  if (dottype==0)
  {g.drawLine(x-s,y,x+s,y);
   g.drawLine(x,y-s,x,y+s);
  }
  else if (dottype==1)
  {
   g.drawOval(x-s,y-s,2*s,2*s);
  }
  else if (dottype==2)
  {
   g.drawLine(x-s,y-s,x+s,y-s);
   g.drawLine(x-s,y+s,x+s,y+s);
   g.drawLine(x-s,y-s,x-s,y+s);
   g.drawLine(x+s,y-s,x+s,y+s);
  }
  else
  {g.drawLine(x-s,y-s,x+s,y+s);
   g.drawLine(x+s,y-s,x-s,y+s);
  }


 }


 public void plotpoints(double[] x,double[] y,int pointsize,String name)
 {
  Color mycolor=linecolors[pointsets%linecolors.length];
  Graphics g=result.getGr();
  g.setColor(mycolor);
  for(int i=0;i<x.length;i++)
  	drawdot(g,getx(x[i]),gety(y[i]),pointsize,pointsets);
  pointsets++;
  if(name!=null)
    result.drawString(name,getx(x[x.length-1])+2,gety(y[x.length-1]),result.DOWNLEFT,result.CENTER);
 }

 public void plotlines(double[] x,double[] y,int pointsize,String name)
 {
  Color mycolor=linecolors[lines%linecolors.length];
  Graphics g=result.getGr();
  g.setColor(mycolor);
  for(int i=1;i<x.length;i++)
  	g.drawLine(getx(x[i-1]),gety(y[i-1]),getx(x[i]),gety(y[i]));
  lines++;
  if(name!=null)
   result.drawString(name,getx(x[x.length-1]),gety(y[x.length-1]),result.DOWNLEFT,result.CENTER);

 }
/******************************************************/
public static void paintonepoint()
{
  Figure fig1=new Figure(320,240,0,1000,1200,1800);
  File f;
  double[][] data;
  f=new File("d:/garesearch/peterb/jobshop/goodresults/bestpopjssel1pno0.txt");
  data=readdata(f);
  fig1.setTitle("Two kinds of 1point","population","makespan");
  f=new File("d:/garesearch/figures/compare1point/bestpopjssel2pno0.txt");
  data=readdata(f);
  fig1.plotlines(data[0],data[1],3,"random avg");
  fig1.plotpoints(data[0],data[2],3,"random min");
  f=new File("d:/garesearch/figures/compare1point/bestpopjssel2pno0alternative.txt");
  data=readdata(f);
  fig1.plotlines(data[0],data[1],3,"nice avg");
  fig1.plotpoints(data[0],data[2],3,"nice min");
  fig1.result.storejpg(new File("d:/garesearch/figures/compare1point/twokinds.jpg"));
  fig1.result.getIC().show();
}




public static void main(String[] ps)
 {
  //paintonepoint();
  //compareAlg();
  //icescaling();
  //icep5();
  uniscaling();
 }

 public static void  compareAlg()
 {
 Figure fig1=new Figure(640,480,0,1000,1200,1800);
 File f;
 double[][] data;
 f=new File("d:/garesearch/figures/comparesevalg/bestpopjssel1pno0.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"uniform avg");
 fig1.plotpoints(data[0],data[2],3,"uniform min");
 f=new File("d:/garesearch/figures/comparesevalg/bestpopjssel2pno0.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"1point avg");
 fig1.plotpoints(data[0],data[2],3,"1point min");
 f=new File("d:/garesearch/figures/comparesevalg/bestpopjssel3pno0.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"ICE avg");
 fig1.plotpoints(data[0],data[2],3,"ICE min");
 f=new File("d:/garesearch/figures/comparesevalg/bestpopjssel4pno0.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"perm.ICE avg");
 fig1.plotpoints(data[0],data[2],3,"perm.ICE min");
 fig1.setTitle("X-over compare","population","makespan");
 fig1.setBottomline("cop. Sieuwert van Otterloo");
 fig1.result.storejpg(new File("d:/garesearch/figures/comparesevalg/xover.jpg"));
 fig1.result.getIC().show();
 }

 public static void  icescaling()
 {
 Figure fig1=new Figure(640,480,0,1200,1200,1800);
 File f;
 double[][] data;
 f=new File("d:/garesearch/figures/icescalingp0/bestpopjssel3pno0e50.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"50 evals avg");
 fig1.plotpoints(data[0],data[2],3,"50 evals min");
 f=new File("d:/garesearch/figures/icescalingp0/bestpopjssel3pno0e100.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"100 evals avg");
 fig1.plotpoints(data[0],data[2],3,"100 evals min");
 f=new File("d:/garesearch/figures/icescalingp0/bestpopjssel3pno0e200.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"200 evals avg");
 fig1.plotpoints(data[0],data[2],3,"200 evals min");
 fig1.setTitle("ICE Scaling behaviour","population","makespan");
 fig1.setBottomline("cop. Sieuwert van Otterloo");
 fig1.result.storejpg(new File("d:/garesearch/figures/uniscaling/uniscaling.jpg"));
 fig1.result.getIC().show();
 }


 public static void  uniscaling()
 {
 Figure fig1=new Figure(640,480,0,1200,1200,1800);
 File f;
 double[][] data;
 f=new File("d:/garesearch/figures/uniscaling/bestpopjsx1p0t50.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"50 evals avg");
 fig1.plotpoints(data[0],data[2],3,"50 evals min");
 f=new File("d:/garesearch/figures/uniscaling/bestpopjsx1p0t100.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"100 evals avg");
 fig1.plotpoints(data[0],data[2],3,"100 evals min");
 f=new File("d:/garesearch/figures/uniscaling/bestpopjsx1p0t150.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"150 evals avg");
 fig1.plotpoints(data[0],data[2],3,"150 evals min");
 f=new File("d:/garesearch/figures/uniscaling/bestpopjsx1p0t200.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"200 evals avg");
 fig1.plotpoints(data[0],data[2],3,"200 evals min");
 fig1.setTitle("Uniform Scaling behaviour","population","makespan");
 fig1.setBottomline("Taillard 1: optimal 1231");
 fig1.result.storejpg(new File("d:/garesearch/figures/uniscaling/uniscaling.jpg"));
 fig1.result.getIC().show();
 }


 public static void  icep5()
 {
 Figure fig1=new Figure(640,480,0,1200,2800,4300);
 File f;
 double[][] data;
 f=new File("d:/garesearch/figures/compareICEp5/bestpopallx3p5.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"ICE avg");
 fig1.plotpoints(data[0],data[2],3,"ICE min");
 f=new File("d:/garesearch/figures/compareICEp5/bestpopjssel1pno5.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"uniform avg");
 fig1.plotpoints(data[0],data[2],3,"uniform min");
 f=new File("d:/garesearch/figures/compareICEp5/bestpopjssel2pno5.txt");
 data=readdata(f);
 fig1.plotlines(data[0],data[1],3,"1point avg");
 fig1.plotpoints(data[0],data[2],3,"1point min");
 fig1.setTitle("performance on Taillard 5","population","makespan");
 fig1.setBottomline("100,000 evaluations");
 fig1.result.storejpg(new File("d:/garesearch/figures/compareICEp5/icep5.jpg"));
 fig1.result.getIC().show();
 }


 static int countlines(File f)
 {
  int lines=0;
  try
  {
  RandomAccessFile raf=new RandomAccessFile(f,"r");
  while(raf.readLine()!=null)
  	lines++;
  raf.close();

  }catch(Exception e)
  {e.printStackTrace();
  }
  return lines;
 }

 /*reads my files. returns popsize, */

 static double[][] readdata(File f)
 {
  int l=countlines(f);
  System.out.println("lines="+l);
  if(l==0)
  	return null;
  double[][] result=new double[4][l];
  try
  {
  RandomAccessFile raf=new RandomAccessFile(f,"r");
  for(int i=0;i<l;i++)
  {
   String line=raf.readLine();
   StringTokenizer tok=new StringTokenizer(line," ><+-",false);
   if (line.startsWith("//"))
   {tok.nextToken();// //
    tok.nextToken();// crossover
	tok.nextToken();// maxeval
	tok.nextToken();// kappa
   }
   tok.nextToken();//problemname
   result[0][i]=Integer.parseInt(tok.nextToken());//x
   tok.nextToken();//dim
   tok.nextToken();//machines
   result[1][i]=Double.parseDouble(tok.nextToken());//avg
   tok.nextToken();//stddev
   result[2][i]=Double.parseDouble(tok.nextToken());//min
   result[3][i]=Double.parseDouble(tok.nextToken());//max
  }
  raf.close();
  }catch(Exception e)
  {e.printStackTrace();
  }
  return result;
 }

}



