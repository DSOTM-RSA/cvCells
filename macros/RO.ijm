function action(input, output, filename) {
        open(input + filename);
        run("Duplicate...", " ");
        setOption("BlackBackground", false);
		run("Make Binary");
		run("Fill Holes");
		run("Create Selection");
		selectWindow(filename);
		run("Restore Selection");

		// choosing ROI Manager
		run("ROI Manager...");
		roiManager("Add");
		roiManager("Split");
		roiManager("Select", 0);
		roiManager("Delete");
		roiManager("Show All");
		roiManager("multi-measure measure_all append");
		saveAs("Results", output + filename + exten);
		close();

		// this section requires multiple objects of interest or Selections
		//roiManager("Select", 3);
		//selectWindow("Profiles");
		//roiManager("Multi Plot");
		//saveAs("Results", output + filename + filename + exten);
       // makeRectangle(10, 10, 300, 180);
        //run("Crop");
       // saveAs("Jpeg", output + filename);
       // close();
}

exten = ".xls";
input = "/home/dan/Documents/GitArchive/cvCells/multiples/images/";
output = "/home/dan/Documents/GitArchive/cvCells/multiples/";


setBatchMode(true); 
list = getFileList(input);
for (i = 0; i < list.length; i++)
        action(input, output, list[i]);
setBatchMode(false);