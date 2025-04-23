import { exec } from "child_process";
import fs from "fs";
import path from "path";
import { parseStringPromise } from "xml2js";
import { v4 as uuidv4 } from "uuid"; // For generating unique IDs

export default async function handler(req, res) {
  // Check HTTP method
  if (req.method !== "POST") {
    return res.status(405).json({ error: "Method Not Allowed" });
  }

  try {
    const {
      sequence,
      program,
      database,
      numDescriptions = 5,
      numAlignments = 5,
      eValue = "1e-2",
      file,
    } = req.body;

    // Input validation
    if (!program || !database) {
      return res.status(400).json({
        error:
          "Missing required parameters: program and database must be specified",
      });
    }

    if (!sequence && (!file || !file.content)) {
      return res.status(400).json({
        error:
          "No sequence provided. Please provide a sequence or upload a file",
      });
    }

    // Validate program type
    const validPrograms = ["blastn", "blastp"];
    if (!validPrograms.includes(program)) {
      return res.status(400).json({
        error: "Invalid BLAST program. Please use blastn or blastp",
      });
    }

    // Validate database
    const validDatabases = [
      "Piper_nigrum_genome_db",
      "Piper_nigrum_cds_db",
      "Piper_nigrum_prot_db",
    ];

    if (!validDatabases.includes(database)) {
      return res.status(400).json({
        error: "Invalid database selected",
      });
    }

    // Validate numeric parameters
    if (isNaN(Number(numDescriptions)) || Number(numDescriptions) <= 0) {
      return res.status(400).json({
        error: "Invalid number of descriptions. Must be a positive number.",
      });
    }

    if (isNaN(Number(numAlignments)) || Number(numAlignments) <= 0) {
      return res.status(400).json({
        error: "Invalid number of alignments. Must be a positive number.",
      });
    }

    // Validate E-value format
    const eValuePattern = /^[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?$/;
    if (!eValuePattern.test(eValue)) {
      return res.status(400).json({
        error:
          "Invalid E-value format. Please use scientific notation (e.g., 1e-2).",
      });
    }

    // Validate program and database compatibility
    const compatibilityCheck = checkProgramDatabaseCompatibility(
      program,
      database
    );
    if (!compatibilityCheck.valid) {
      return res.status(400).json({
        error: compatibilityCheck.message,
      });
    }

    // Check sequence type compatibility with program
    let sequenceContent = sequence;
    if (file && file.content) {
      try {
        sequenceContent = Buffer.from(file.content, "base64").toString("utf8");
      } catch (error) {
        return res.status(400).json({
          error: "Invalid file content encoding. File must be base64 encoded.",
        });
      }
    }

    const sequenceTypeCheck = checkSequenceTypeCompatibility(
      program,
      sequenceContent
    );
    if (!sequenceTypeCheck.valid) {
      return res.status(400).json({
        error: sequenceTypeCheck.message,
      });
    }

    // Count sequences in FASTA input
    const sequenceCount = countFastaSequences(sequenceContent);
    if (sequenceCount > 5) {
      return res.status(400).json({
        error:
          "Too many sequences. Maximum 5 sequences allowed per submission.",
      });
    }

    // Define BLAST database path
    const blastDbPath = path.join(
      "D:",
      "Projects_nextjs",
      "pepper-website",
      "Blast_DB",
      database
    );

    // Check if database exists
    if (
      !fs.existsSync(`${blastDbPath}.nin`) &&
      !fs.existsSync(`${blastDbPath}.pin`)
    ) {
      return res.status(500).json({
        error: "Database not found. Please contact the administrator.",
      });
    }

    // Generate a unique ID for this query
    const queryId = uuidv4();
    const tempDir = path.join(process.cwd(), "temp", queryId);

    try {
      fs.mkdirSync(tempDir, { recursive: true });
    } catch (error) {
      console.error(`Error creating temporary directory: ${error.message}`);
      return res.status(500).json({
        error: "Failed to create temporary storage for BLAST query",
      });
    }

    // Define paths for query and result files
    const queryFilePath = path.join(tempDir, "query.fasta");
    const resultsFilePath = path.join(tempDir, "results.xml");

    try {
      if (file && file.content) {
        // Handle file upload - save the uploaded file content
        try {
          const fileBuffer = Buffer.from(file.content, "base64");
          fs.writeFileSync(queryFilePath, fileBuffer);
        } catch (error) {
          throw new Error(`Failed to process uploaded file: ${error.message}`);
        }
      } else if (sequence) {
        // If no file, use the provided sequence
        fs.writeFileSync(queryFilePath, sequence);
      }

      // Construct the BLAST command with sanitized inputs
      const command = `${program} -db ${blastDbPath} -query ${queryFilePath} -out ${resultsFilePath} -outfmt 5 -num_descriptions ${parseInt(
        numDescriptions
      )} -num_alignments ${parseInt(numAlignments)} -evalue ${eValue}`;

      // Execute BLAST command
      exec(command, async (error, stdout, stderr) => {
        try {
          if (error) {
            console.error(`Error executing BLAST command: ${stderr}`);
            throw new Error(stderr || "Failed to execute BLAST search");
          }

          // Check if results file exists and has content
          if (!fs.existsSync(resultsFilePath)) {
            throw new Error("BLAST process did not generate results file");
          }

          if (fs.statSync(resultsFilePath).size === 0) {
            throw new Error(
              "No results found. Please check your sequence and parameters."
            );
          }

          const xmlResults = fs.readFileSync(resultsFilePath, "utf-8");
          const parsedResults = await parseBlastXML(xmlResults);

          if (parsedResults.hits.length === 0) {
            parsedResults.message =
              "No significant matches found. Try adjusting your search parameters.";
          }

          res.status(200).json({
            results: parsedResults,
            programDatabase: {
              program: program,
              database: database,
            },
          });
        } catch (processError) {
          console.error(
            `Error processing BLAST results: ${processError.message}`
          );
          let errorMessage = processError.message;

          // Extract more specific error information if available
          if (stderr && stderr.includes("BLAST engine error")) {
            errorMessage =
              "BLAST engine error. Please check your sequence and parameters.";
          }

          res.status(500).json({ error: errorMessage });
        } finally {
          // Always clean up temporary files
          cleanupTempFiles(tempDir);
        }
      });
    } catch (err) {
      console.error(`Unexpected error: ${err.message}`);
      // Clean up temporary files if directory exists
      cleanupTempFiles(tempDir);
      res.status(500).json({ error: "An unexpected error occurred" });
    }
  } catch (uncaughtError) {
    console.error(`Uncaught API error: ${uncaughtError.message}`);
    res.status(500).json({ error: "Internal server error" });
  }
}

// Function to clean up temporary files
function cleanupTempFiles(dirPath) {
  try {
    if (fs.existsSync(dirPath)) {
      fs.rmdirSync(dirPath, { recursive: true });
    }
  } catch (error) {
    console.error(`Error cleaning up temporary files: ${error.message}`);
  }
}

// Function to count sequences in FASTA format
function countFastaSequences(content) {
  if (!content) return 0;
  // Count occurrences of '>' character at the beginning of a line
  return (content.match(/^>/gm) || []).length;
}

// Function to check if the program and database selection are compatible
function checkProgramDatabaseCompatibility(program, database) {
  // For blastn, nucleotide databases are compatible
  if (program === "blastn") {
    if (database === "Piper_nigrum_prot_db") {
      return {
        valid: false,
        message:
          "Incompatible selection: blastn cannot be used with protein databases. Please use a nucleotide database or switch to blastp.",
      };
    }
  }

  // For blastp, protein databases are compatible
  if (program === "blastp") {
    if (
      database === "Piper_nigrum_genome_db" ||
      database === "Piper_nigrum_cds_db"
    ) {
      return {
        valid: false,
        message:
          "Incompatible selection: blastp cannot be used with nucleotide databases. Please use a protein database or switch to blastn.",
      };
    }
  }

  return { valid: true };
}

// Function to check if sequence type is compatible with the selected program
function checkSequenceTypeCompatibility(program, sequenceContent) {
  if (!sequenceContent) {
    return { valid: true }; // Skip check if no sequence content
  }

  // Attempt to determine if the sequence is protein or nucleotide
  const isProteinLike = determineSequenceType(sequenceContent);

  if (program === "blastn" && isProteinLike) {
    return {
      valid: false,
      message:
        "Your sequence appears to be a protein sequence, but you selected blastn. Use blastp for protein sequences.",
    };
  }

  if (program === "blastp" && !isProteinLike) {
    return {
      valid: false,
      message:
        "Your sequence appears to be a nucleotide sequence, but you selected blastp. Use blastn for nucleotide sequences.",
    };
  }

  return { valid: true };
}

// Helper function to determine if sequence is likely protein or nucleotide
function determineSequenceType(sequence) {
  // Remove FASTA header lines and whitespace
  const cleanedSequence = sequence
    .split("\n")
    .filter((line) => !line.startsWith(">"))
    .join("")
    .replace(/\s/g, "")
    .toUpperCase();

  if (cleanedSequence.length === 0) return false;

  // Count non-nucleotide characters (not A, T, G, C, N)
  const nonNucleotideChars = cleanedSequence.replace(/[ATGCN]/g, "");

  // If more than 10% of characters are non-nucleotide, likely a protein sequence
  return nonNucleotideChars.length / cleanedSequence.length > 0.1;
}

async function parseBlastXML(xmlResults) {
  try {
    const json = await parseStringPromise(xmlResults);

    // Check if we have valid BLAST output
    if (!json.BlastOutput) {
      throw new Error("Invalid BLAST output format");
    }

    // Extracting iteration hits
    const hits =
      json.BlastOutput?.BlastOutput_iterations?.[0]?.Iteration?.[0]?.Iteration_hits?.[0]?.Hit?.map(
        (hit) => {
          const hsps = hit.Hit_hsps?.[0]?.Hsp || [];

          // Mapping HSPs to extract query, hit sequences, midline, and alignment length
          const alignments = hsps.map((hsp) => ({
            querySeq: hsp.Hsp_qseq?.[0] || "N/A",
            hitSeq: hsp.Hsp_hseq?.[0] || "N/A",
            midline: hsp.Hsp_midline?.[0] || "N/A",
            alignLength: hsp["Hsp_align-len"]?.[0] || "N/A",
            queryFrom: hsp["Hsp_query-from"]?.[0] || "N/A",
            queryTo: hsp["Hsp_query-to"]?.[0] || "N/A",
            hitFrom: hsp["Hsp_hit-from"]?.[0] || "N/A",
            hitTo: hsp["Hsp_hit-to"]?.[0] || "N/A",
            gaps: hsp["Hsp_gaps"]?.[0] || "N/A",
            identity: hsp["Hsp_identity"]?.[0] || "N/A",
            bitScore: hsp["Hsp_bit-score"]?.[0] || "N/A",
          }));

          return {
            id: hit.Hit_id?.[0] || "N/A",
            description: hit.Hit_def?.[0] || "N/A",
            score: hit.Hit_hsps?.[0]?.Hsp?.[0]?.Hsp_score?.[0] || "N/A",
            evalue: hit.Hit_hsps?.[0]?.Hsp?.[0]?.Hsp_evalue?.[0] || "N/A",
            alignLength:
              alignments.length > 0 ? alignments[0].alignLength : "N/A",
            alignments:
              alignments.length > 0
                ? alignments
                : [
                    {
                      querySeq: "N/A",
                      hitSeq: "N/A",
                      midline: "N/A",
                      alignLength: "N/A",
                      queryFrom: "N/A",
                      queryTo: "N/A",
                      hitFrom: "N/A",
                      hitTo: "N/A",
                      gaps: "N/A",
                      identity: "N/A",
                      bitScore: "N/A",
                    },
                  ],
          };
        }
      ) || [];

    // Extracting statistics
    const statistics =
      json.BlastOutput?.BlastOutput_iterations?.[0]?.Iteration?.[0]
        ?.Iteration_stat?.[0]?.Statistics?.[0] || {};

    const stats = {
      dbNum: statistics["Statistics_db-num"]?.[0] || "N/A",
      dbLen: statistics["Statistics_db-len"]?.[0] || "N/A",
      hspLen: statistics["Statistics_hsp-len"]?.[0] || "N/A",
      effSpace: statistics["Statistics_eff-space"]?.[0] || "N/A",
      kappa: statistics["Statistics_kappa"]?.[0] || "N/A",
      lambda: statistics["Statistics_lambda"]?.[0] || "N/A",
      entropy: statistics["Statistics_entropy"]?.[0] || "N/A",
    };

    // Get program and database information
    const program = json.BlastOutput?.BlastOutput_program?.[0] || "N/A";
    const database = json.BlastOutput?.BlastOutput_db?.[0] || "N/A";

    return {
      hits,
      stats,
      program,
      database,
      message:
        hits.length > 0 ? "Search completed successfully." : "No hits found.",
    };
  } catch (error) {
    console.error(`Error parsing XML: ${error.message}`);
    throw new Error("Failed to parse BLAST results");
  }
}
