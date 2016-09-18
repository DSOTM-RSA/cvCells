function action(input, output, filename) {
        open(input + filename);
        run("Duplicate...", " ");
        setOption("BlackBackground", false);
		run("Make Binary");
		run("Fill Holes");
        makeRectangle(10, 10, 300, 180);
        run("Crop");
        saveAs("Jpeg", output + filename);
        close();
}

// this function successfully opens required images, converts to binary,
// fills, anc crops, and saves the output file.



input = "/home/dan/Documents/GitArchive/cvCells/singles/images/";
output = "/home/dan/Documents/GitArchive/cvCells/singles/";

// input and output directories are defined above


setBatchMode(true); 
list = getFileList(input);
for (i = 0; i < list.length; i++)
        action(input, output, list[i]);
setBatchMode(false);

// batch mode is set to true