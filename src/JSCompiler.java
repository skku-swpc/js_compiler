import java.util.ArrayList;

import java.io.IOException;
import java.io.File;


@SuppressWarnings("unchecked")
public class JSCompiler {
	private String inputFile = "";
	private String outputFile = "";

	public JSCompiler (String inputFile, String outputFile) {
		this.inputFile = inputFile;
		this.outputFile = outputFile;
	}

	public JSCompiler () {
		this ("input.js", "output.ll");
	}

	public void setInputFile (String inputFile) {
		this.inputFile = inputFile;
	}
	
	public void setOutputFile (String outputFile) {
		this.outputFile = outputFile;
	}

	public void run () throws IOException {
		Runtime rt = Runtime.getRuntime ();
		String[] cmd = new String[4];

		cmd[0] = "python";
		cmd[1] = "ECMAScriptMain.py";
		cmd[2] = this.inputFile;
		cmd[3] = this.outputFile;

		rt.exec (cmd);
	}

}
