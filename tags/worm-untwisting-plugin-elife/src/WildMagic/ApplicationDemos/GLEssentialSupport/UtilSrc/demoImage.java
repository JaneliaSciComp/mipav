package WildMagic.ApplicationDemos.GLEssentialSupport.UtilSrc;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.net.URL;

import javax.imageio.ImageIO;
import javax.media.opengl.GL;

public class demoImage
{
	public byte[] data;

	public int width;
	public int height;
	public int format;
	public int type;

	public int rowByteSize;


	public demoImage( String filepathname, String fileName, boolean flipVertical )
	{
		URL fileURL = getClass().getClassLoader().getResource(filepathname);
		if ( fileURL != null )
		{
			String path = fileURL.getPath().substring( 0, fileURL.getPath().lastIndexOf("/") + 1 );
			//System.err.println( path );
			BufferedImage inputImage = null;
			try {
				inputImage = ImageIO.read( new File(path + filepathname) );
				//inputImage = ImageIO.read( new File(path + fileName) );

				init(inputImage);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}  finally { }
		}
	}

	public demoImage( BufferedImage input )
	{
		init(input);
	}
	
	public void init( BufferedImage input )
	{
		width = input.getWidth(null);
		height = input.getHeight(null);
		rowByteSize = width * 4;
		data = new byte[ height * rowByteSize ];
		format = GL.GL_RGBA;
		type = GL.GL_UNSIGNED_BYTE;

		int i = 0;
		for (int y = 0; y < height; y++) {

			for (int x = 0; x < width; x++) {
				int pixel = input.getRGB(x, y);
				int a = (pixel >> 24) & 0xff;
				int r = (pixel >> 16) & 0xff;
				int g = (pixel >> 8) & 0xff;
				int b = (pixel) & 0xff;

				data[i + 0] = (byte) r;
				data[i + 1] = (byte) g;
				data[i + 2] = (byte) b;
				data[i + 3] = (byte) a;
				i += 4;
			}
		}

	}

	public void dispose( )
	{
		data = null;
	}

};

