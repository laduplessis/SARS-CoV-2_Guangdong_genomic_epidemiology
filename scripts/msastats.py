import os, sys, time, collections
import numpy as np
from optparse import OptionParser
from Bio import AlignIO, SeqIO

# Given an alignment and a reference sequence find location of all mutations (SNPs) and gaps in sequences wrt the reference
# Also return the base composition (histogram of nucleotides) for each sequence in the alignment
################################################################################################################################
#
# Can be used as a standalone program or functions can be called from other Python scripts
# For help as a standalone program type: python msastats.py -h
#
# Examples:
#	1.) python msastats.py -i msa.fas 
#
#		Extract SNPs and gaps in msa.fas wrt the first sequence in msa.fas
#		Results are stored in the current directory
#		Fields in the sequence ids in msa.fas are separated by | and the first field is used (seq1|abc|123 becomes seq1)
#
#	2.) python msastats.py -i msa.fas -r ref.fa -o output -s 1,2 -f "_"
#	
#		Extract SNPs and gaps in sequences in msa.fas wrt ref.fa
#		Results are stored in output/
#		Fields in the sequence ids in msa.fas are separated by _ and the second and third fields are used 
#		(seq1_abc_123 becomes abc_123)
#
#
# Notes:
#    -  Everything is calculated against the reference. All positions where the reference sequence has a gap are ignored! 
#	 -  If the reference is taken as the first sequence in the alignment and the reference contains gaps those sites are treated
#		as either SNPs (sequence doesn't have a gap at those sites) or covered (sequence also has a gap).
#	 -  Case is ignored (t == T and a == A etc.) and all results are returned in uppercase.
# 
################################################################################################################################



################################################################################################################################
# SNPs

def snpScanner(reference, sequence, ambiguousAreGaps=False):
	"""
	Find all SNPs wrt to the reference

	if (ambiguousAreGaps):
		All ambiguous nucleotides are treated as gaps
	else
		Assumes that N, - and _ are the only gap characters

	"""


	if (len(reference) != len(sequence)):
		sys.stdout.write("Error! Sequence lengths differ! Exiting...\n")
		sys.exit()


	if (ambiguousAreGaps):
		gapchars  = ["_", "-", "R", "Y", "W", "S", "K", "M", "D", "V", "H", "B"]
	else:
		gapchars  = ["_", "-"]

	reference = str(reference).upper()
	sequence  = str(sequence).upper()
	for c in gapchars:
		reference = reference.replace(c,"N")
		sequence  = sequence.replace(c, "N")


	snps = []
	for i in range(0, len(reference)):

		if (sequence[i] != reference[i] and 
			sequence[i] != "N"):
			snps.append(i)

	return(snps)
#


def saveSnps(fname, ref, seq, snps):


	outfile = open(fname,"w")
	outfile.write("# Position numbering starts at 1\n")	
	outfile.write(",".join(["position",ref.id,seq.id])+"\n")   
	for i in snps:		
		outfile.write("%d,%s,%s\n" % (i+1, ref.seq[i], seq.seq[i]))

	outfile.close()
#


################################################################################################################################
# Gaps

def gapScanner(reference, sequence, ambiguousAreGaps=False):
	"""
	Find all gaps in the sequence wrt the reference

	if (ambiguousAreGaps):
		All ambiguous nucleotides are treated as gaps
	else
		Assumes that N, - and _ are the only gap characters
	"""

	if (len(reference) != len(sequence)):
		sys.stdout.write("Error! Sequence lengths differ! Exiting...\n")
		sys.exit()


	if (ambiguousAreGaps):
		gapchars  = ["_", "-", "R", "Y", "W", "S", "K", "M", "D", "V", "H", "B"]
	else:
		gapchars  = ["_", "-"]

	reference = str(reference).upper()
	sequence  = str(sequence).upper()
	for c in gapchars:
		reference = reference.replace(c,"N")
		sequence  = sequence.replace(c, "N")


	gap        = 0
	gapStarts  = []
	gapEnds    = []
	gapLengths = []
	i = 0
	while (i < len(reference)):

		if (sequence[i] == "N" and reference[i] != "N"):
			gap += 1
			if (gap == 1):
				#sys.stdout.write("\tOPEN GAP: %d\t%s\t%s\n" % (i, sequence[i], reference[i]))
				gapStarts.append(i)
		else: 
			if (gap > 0):
				#sys.stdout.write("\tCLOSE GAP: %d\t%s\t%s\n" % (i, sequence[i], reference[i]))
				gapEnds.append(i-1)
				gapLengths.append(gap)
				gap = 0	
			
		i += 1

	if (gap > 0):
		gapEnds.append(i-1)
		gapLengths.append(gap)
		gap = 0	

	return (gapStarts, gapEnds, gapLengths)
#
	


def saveGaps(fname, gapStarts, gapEnds, gapLengths):

	outfile = open(fname,"w")
	outfile.write("# Position numbering starts at 1\n")
	outfile.write(",".join(["start", "end", "length"])+"\n")   
	for i in range(0, len(gapStarts)):		
		if (gapEnds[i] - gapStarts[i] != gapLengths[i]-1):
			sys.stdout.write("Error! Gap length calculated wrongly!\n")
			sys.exit()

		outfile.write("%d,%d,%d\n" % (gapStarts[i]+1, gapEnds[i]+1, gapLengths[i]))

	outfile.close()
#


################################################################################################################################
# Histogram

def seqhist(msa, alphabet, normalise=False):
	
	histmat = np.zeros([len(msa), len(alphabet)], dtype=float if normalise else int)	

	seqids = []
	for i in range(0, len(msa)):
		seq = msa[i].seq

	 	# Faster way
		seqdict = collections.Counter(seq.upper())
	 	
	 	# Mising characters not in alphabet
		for c in seqdict.keys():
			if (c not in alphabet):
				sys.stdout.write(c+" not in alphabet!\n")

		total = 0		
		for j in range(0,len(alphabet)):
			c = alphabet[j]
			if (c in seqdict.keys()):
				histmat[i,j] = seqdict[c]
				total += seqdict[c]						

		if (normalise):
			histmat[i,:] /= total

		seqids.append(msa[i].id)
	#
	return (seqids, histmat)
#

def saveseqhist(hist, ids, alphabet, fname):

	outfile = open(fname,"w")
	outfile.write("id,"+",".join(alphabet)+"\n")   
	for i in range(0,hist.shape[0]):
		outfile.write(ids[i]+","+",".join(map(str, hist[i,:]))+"\n")
	outfile.close()





####################################################################################################################################

if __name__ == "__main__":

	################################################################################################################################
	# Parameters
	################################################################################################################################
	usage = "usage: %prog [option]"
	parser = OptionParser(usage=usage)

	parser.add_option("-i","--inputfile",
	                  dest = "inputfile",
	                  default = "",
	                  metavar = "path",
	                  help = "Path to input alignment [required]")

	parser.add_option("-r","--reference",
	                  dest = "reference",
	                  default = "",
	                  metavar = "path",
	                  help = "Path to reference sequence (if blank the first sequence in the alignment will be used) [required]")

	parser.add_option("-o","--outputpath",
	                  dest = "outputpath",
	                  default = "",
	                  metavar = "path",
	                  help = "Path to save output files in [required]")

	parser.add_option("-t","--filetype",
	                  dest = "type",
	                  default = "fasta",
	                  metavar = "string",
	                  help = "File format of sequence files [default: %default] [optional]")

	parser.add_option("-f","--fastasep",
	                  dest = "fasep",
	                  default = "|",
	                  metavar = "string",
	                  help = "Separator between fields in fasta sequence id's [default: %default] [optional]")

	parser.add_option("-s","--seqidfields",
	                  dest = "seqidfields",
	                  default = "0",
	                  metavar = "comma-separated list of strings",
	                  help = "Comma-separated list of fields in the alignment sequence ids to use in the output filenames (numbered from 0),"
	                  		 "by default uses the first field [optional]")

	parser.add_option("-a","--alphabet",
	                  dest = "alphabet",
	                  default = "A,C,G,T,R,Y,W,S,K,M,D,V,H,B,N,-,_",
	                  metavar = "string",
	                  help = "Comma-separated list of all characters (nucleotides) allowed in an alignment [default: %default] [optional]")

	parser.add_option("-n","--normalise",
	                  dest = "normalise",
	                  action = "store_true",
	                  help = "Normalise the counts in the histogram [default: %default] [optional]")

	parser.add_option("-g","--ambiguousAreGaps",
	                  dest = "ambiguousAreGaps",
	                  action = "store_true",
	                  help = "If true all partially ambiguous characters are treated as gaps [default: %default] [optional]")

	(options,args) = parser.parse_args()

	inputfile     = os.path.abspath(options.inputfile)
	reference     = os.path.abspath(options.reference) if options.reference != "" else ""
	outputpath    = os.path.abspath(options.outputpath)+"/"
	filetype      = options.type
	fasep         = options.fasep
	seqidfields   = [] if options.seqidfields == "" else [int(i) for i in options.seqidfields.split(",")]
	normalise     = options.normalise
	alphabet      = options.alphabet.split(",")
	ambiguousGaps = options.ambiguousAreGaps
	#alphabet   = ["A", "C", "G", "T", "R", "Y", "W", "S", "K", "M", "D", "V", "H", "B", "N", "-"]

	start = time.time()

	msa = AlignIO.read(inputfile, filetype)
	for i in range(0, len(msa)):
		msa[i].seq = msa[i].seq.upper()



	if (reference != ""):
		ref = SeqIO.read(reference, filetype).upper()
		refid = ref.id
	else:
		ref = msa[0]
		refidparts = [ref.id.split(fasep)[j] for j in seqidfields]
		refid      = fasep.join(refidparts)

	if (not os.path.exists(outputpath)):
	    os.mkdir(outputpath)


	for i in range(0, len(msa)):
		seq = msa[i]	
		sys.stdout.write(seq.id+"\t")
		
		seqidparts = [seq.id.split(fasep)[j] for j in seqidfields]
		fileid     = fasep.join(seqidparts)
		sys.stdout.write(fileid+"\t")

		# Get SNPs
		snps = snpScanner(ref.seq, seq.seq, ambiguousAreGaps=ambiguousGaps)
		sys.stdout.write("%5d SNPs" % len(snps))
		saveSnps(outputpath + refid + "_" + fileid + ".snps.csv", ref, seq, snps)

		# Get gaps
		(gapStarts, gapEnds, gapLengths) = gapScanner(ref.seq, seq.seq, ambiguousAreGaps=ambiguousGaps)
		sys.stdout.write("%5d gaps\n" % len(gapStarts))
		saveGaps(outputpath + refid + "_" + fileid + ".gaps.csv", gapStarts, gapEnds, gapLengths)
		
	# Get base composition
	(seqids, histmat) = seqhist(msa, alphabet, normalise=normalise)
	saveseqhist(histmat, seqids, alphabet, outputpath+inputfile[inputfile.rfind("/")+1:inputfile.rfind(".")]+".seqhist.csv")

	end = time.time()
	sys.stdout.write("Total time taken: "+str(end-start)+" seconds\n")

