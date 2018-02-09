package WildMagic.ApplicationDemos.GLEssentialSupport.UtilSrc;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;

import javax.media.opengl.GL;
import javax.media.opengl.GL2;
public class demoSource
{
	public String shaderProgram;
	public int shaderType;

	public demoSource( final String filepathname, String fileNameInput )
	{
        String fileName = fileNameInput.trim();
        int index = fileName.lastIndexOf(".");

        if (fileName.substring(index + 1).equalsIgnoreCase("fsh"))
        {
        	shaderType = GL2.GL_FRAGMENT_SHADER;
        }
        else if (fileName.substring(index + 1).equalsIgnoreCase("vsh"))
        {
        	shaderType = GL2.GL_VERTEX_SHADER;
        }
        else
        {
        	// Unknown suffix
        	shaderType = 0;
        }
		URL fileURL = getClass().getClassLoader().getResource(filepathname);
		String path = fileURL.getPath().substring( 0, fileURL.getPath().lastIndexOf("/") + 1 );
		//System.err.println( path );
		
		if ( fileURL != null )
		{
			BufferedReader input = null;
			try {

				input = new BufferedReader(new InputStreamReader(fileURL.openStream()));
				String content = "";
				String line = null;

				while ( (line = input.readLine()) != null) {
					content += line + "\n";
				}
				shaderProgram = new String(content);
			} catch (FileNotFoundException kFNF) {
				System.err.println("Unable to find the shader file " + filepathname);
			} catch (IOException kIO) {
				System.err.println("Problem reading the shader file " + filepathname);
			} finally {
				try {
					if (input != null) {
						input.close();
					}
				} catch (IOException closee) {}
			}
		}
	}

	public void dispose()
	{
		shaderProgram = null;
	}
}