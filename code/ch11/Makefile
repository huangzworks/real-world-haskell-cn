#Just a basic makefile to make life easier.

Run : QC.hs Prettify2.hs Run.hs;ghc --make -fhpc Run.hs

Run.tix : Run; rm -f Run.tix; ./Run

report : Run.tix; hpc report Run --exclude=Main --exclude=QC

markup : Run.tix; hpc markup Run --exclude=Main --exclude=QC

clean : ;rm -rf Run *.hi *.o *.html *.tix .hpc