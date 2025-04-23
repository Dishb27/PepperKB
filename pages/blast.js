import Head from "next/head";
import React, { useState, useEffect } from "react";
import {
  FaSpinner,
  FaChevronDown,
  FaChevronUp,
  FaUpload,
} from "react-icons/fa";
import styles from "../styles/blast.module.css";
import { useRouter } from "next/router";
import Header from "../components/header";
import Footer from "../components/footer";

const Blast = () => {
  const router = useRouter();
  const [formData, setFormData] = useState({
    sequence: "",
    program: "blastn",
    database: "",
    numDescriptions: 5,
    numAlignments: 5,
    eValue: "1e-2",
  });
  const [file, setFile] = useState(null);
  const [fileContent, setFileContent] = useState(null);
  const [advancedOptions, setAdvancedOptions] = useState(false);
  const [notification, setNotification] = useState({ message: "", type: "" });
  const [isLoading, setIsLoading] = useState(false);
  const [fileError, setFileError] = useState("");
  const [errors, setErrors] = useState({});
  const [fileUploaded, setFileUploaded] = useState(false);
  const [sequenceType, setSequenceType] = useState(null);

  // Clear notification after 5 seconds
  useEffect(() => {
    if (notification.message) {
      const timer = setTimeout(() => {
        setNotification({ message: "", type: "" });
      }, 5000);
      return () => clearTimeout(timer);
    }
  }, [notification]);

  // Effect to validate sequence type when program changes
  useEffect(() => {
    if (sequenceType && formData.program) {
      validateSequenceTypeWithProgram(formData.program, sequenceType);
    }
  }, [formData.program, sequenceType]);

  // Effect to validate database compatibility when program or database changes
  useEffect(() => {
    if (formData.program && formData.database) {
      validateProgramDatabaseCompatibility(formData.program, formData.database);
    }
  }, [formData.program, formData.database]);

  const toggleAdvancedOptions = () => {
    setAdvancedOptions(!advancedOptions);
  };

  const handleInputChange = (e) => {
    const { name, value } = e.target;
    setFormData({ ...formData, [name]: value });

    // Clear related error when user makes changes
    if (errors[name]) {
      setErrors({ ...errors, [name]: "" });
    }

    // Check sequence type when user inputs a sequence
    if (name === "sequence" && value) {
      const detected = determineSequenceType(value);
      setSequenceType(detected);

      // If program is already selected, validate compatibility
      if (formData.program) {
        validateSequenceTypeWithProgram(formData.program, detected);
      }
    } else if (name === "sequence" && !value) {
      setSequenceType(null);
      setErrors({ ...errors, sequence: "" });
    }
  };

  const handleFileUpload = (e) => {
    const selectedFile = e.target.files[0];
    if (!selectedFile) {
      setFile(null);
      setFileContent(null);
      setFileUploaded(false);
      setSequenceType(null);
      setErrors({ ...errors, sequence: "" });
      return;
    }

    // Validate file type
    const validFileTypes = [".fasta", ".fa", ".fna"];
    const fileExtension = selectedFile.name
      .substring(selectedFile.name.lastIndexOf("."))
      .toLowerCase();

    if (!validFileTypes.includes(fileExtension)) {
      setFileError(
        "Invalid file type. Please upload a FASTA file (.fasta, .fa, .fna)"
      );
      setFile(null);
      setFileContent(null);
      setFileUploaded(false);
      return;
    }

    // File size validation (max 2MB)
    const maxSize = 2 * 1024 * 1024; // 2MB
    if (selectedFile.size > maxSize) {
      setFileError("File is too large. Maximum size is 2MB.");
      setFile(null);
      setFileContent(null);
      setFileUploaded(false);
      return;
    }

    // Read file content and convert to base64
    const reader = new FileReader();
    reader.onload = (event) => {
      const fileContent = event.target.result;
      // Get the base64 part of the data URL
      const base64Content = fileContent.split(",")[1];
      setFileContent(base64Content);

      // Check the file content to determine if it's protein or nucleotide
      const textContent = atob(base64Content);
      const detected = determineSequenceType(textContent);
      setSequenceType(detected);

      // If program is already selected, validate compatibility
      if (formData.program) {
        validateSequenceTypeWithProgram(formData.program, detected);
      }

      setFileError("");
      setFile(selectedFile);
      setFileUploaded(true);

      // Clear sequence input if there was any
      if (formData.sequence) {
        setFormData({ ...formData, sequence: "" });
      }
    };

    reader.onerror = () => {
      setFileError("Failed to read file. Please try again.");
      setFile(null);
      setFileContent(null);
      setFileUploaded(false);
    };

    // Read as data URL to get base64 encoding
    reader.readAsDataURL(selectedFile);
  };

  // Helper function to determine if sequence is protein or nucleotide
  const determineSequenceType = (sequence) => {
    if (!sequence) return null;

    // Remove FASTA header lines and whitespace
    const cleanedSequence = sequence
      .split("\n")
      .filter((line) => !line.startsWith(">"))
      .join("")
      .replace(/\s/g, "")
      .toUpperCase();

    if (cleanedSequence.length === 0) return null;

    // Count non-nucleotide characters (not A, T, G, C, N)
    const nonNucleotideChars = cleanedSequence.replace(/[ATGCN]/g, "");

    // If more than 10% of characters are non-nucleotide, likely a protein sequence
    return nonNucleotideChars.length / cleanedSequence.length > 0.1
      ? "protein"
      : "nucleotide";
  };

  // Validate sequence type with program
  const validateSequenceTypeWithProgram = (program, type) => {
    if (!type || !program) return true;

    if (program === "blastn" && type === "protein") {
      setErrors({
        ...errors,
        sequence:
          "Protein sequences cannot be used with blastn. Please use blastp for protein sequences or provide a nucleotide sequence.",
      });
      return false;
    }

    if (program === "blastp" && type === "nucleotide") {
      setErrors({
        ...errors,
        sequence:
          "Nucleotide sequences cannot be used with blastp. Please use blastn for nucleotide sequences or provide a protein sequence.",
      });
      return false;
    }

    // Clear error if types match
    setErrors({ ...errors, sequence: "" });
    return true;
  };

  // Validate program and database compatibility
  const validateProgramDatabaseCompatibility = (program, database) => {
    if (!program || !database) return true;

    // For blastn, protein databases are incompatible
    if (program === "blastn" && database === "Piper_nigrum_prot_db") {
      setErrors({
        ...errors,
        database:
          "blastn cannot be used with protein databases. Please select a nucleotide database or switch to blastp.",
      });
      return false;
    }

    // For blastp, nucleotide databases are incompatible
    if (
      program === "blastp" &&
      (database === "Piper_nigrum_genome_db" ||
        database === "Piper_nigrum_cds_db")
    ) {
      setErrors({
        ...errors,
        database:
          "blastp cannot be used with nucleotide databases. Please select a protein database or switch to blastn.",
      });
      return false;
    }

    // Clear error if compatible
    setErrors({ ...errors, database: "" });
    return true;
  };

  const validateForm = () => {
    const newErrors = {};

    // Validate database selection
    if (!formData.database) {
      newErrors.database = "Please select a database";
    }

    // Validate sequence input or file upload
    if (!formData.sequence && !file) {
      newErrors.sequence = "Please enter a sequence or upload a file";
    }

    // Validate sequence count if sequence is provided
    if (formData.sequence) {
      const sequenceCount = formData.sequence.split(">").length - 1;
      if (sequenceCount > 5) {
        newErrors.sequence = "You can only submit a maximum of 5 sequences";
      }
    }

    // Validate e-value format
    const eValuePattern = /^[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?$/;
    if (!eValuePattern.test(formData.eValue)) {
      newErrors.eValue = "Please enter a valid e-value (e.g., 1e-2)";
    }

    // Validate numerical inputs
    if (formData.numDescriptions <= 0) {
      newErrors.numDescriptions = "Value must be greater than 0";
    }

    if (formData.numAlignments <= 0) {
      newErrors.numAlignments = "Value must be greater than 0";
    }

    // Check sequence type compatibility with program
    if (sequenceType) {
      const isCompatible = validateSequenceTypeWithProgram(
        formData.program,
        sequenceType
      );
      if (!isCompatible && !newErrors.sequence) {
        newErrors.sequence = errors.sequence;
      }
    }

    // Check program-database compatibility
    if (formData.program && formData.database) {
      const isCompatible = validateProgramDatabaseCompatibility(
        formData.program,
        formData.database
      );
      if (!isCompatible && !newErrors.database) {
        newErrors.database = errors.database;
      }
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();

    if (!validateForm()) {
      setNotification({
        message: "Please fix the errors before submitting",
        type: "error",
      });
      return;
    }

    try {
      setIsLoading(true);

      // Prepare form data
      const blastData = {
        sequence: formData.sequence,
        program: formData.program,
        database: formData.database,
        numDescriptions: parseInt(formData.numDescriptions),
        numAlignments: parseInt(formData.numAlignments),
        eValue: formData.eValue,
      };

      // Add file data in the format expected by the API if a file is uploaded
      if (file && fileContent) {
        blastData.file = {
          name: file.name,
          content: fileContent, // Base64 encoded content
        };
      }

      // Send request to the API
      const response = await fetch("/api/blast", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(blastData),
      });

      const data = await response.json();

      if (!response.ok) {
        throw new Error(data.error || "Failed to submit BLAST query");
      }

      // Redirect to results page
      router.push({
        pathname: "/resultblast",
        query: {
          results: JSON.stringify(data.results),
          program: formData.program,
          database: formData.database,
        },
      });
    } catch (error) {
      console.error("Error submitting BLAST query:", error);
      setNotification({
        message:
          error.message || "An error occurred while processing your request",
        type: "error",
      });
    } finally {
      setIsLoading(false);
    }
  };

  const handleClearForm = () => {
    setFormData({
      sequence: "",
      program: "blastn",
      database: "",
      numDescriptions: 5,
      numAlignments: 5,
      eValue: "1e-2",
    });
    setFile(null);
    setFileContent(null);
    setFileUploaded(false);
    setErrors({});
    setFileError("");
    setSequenceType(null);
  };

  // Helper function to get database options based on selected program
  const getDatabaseOptions = () => {
    if (formData.program === "blastn") {
      return (
        <>
          <option value="">- Choose a database -</option>
          <option value="Piper_nigrum_genome_db">
            Piper nigrum genome database
          </option>
          <option value="Piper_nigrum_cds_db">Piper nigrum CDS database</option>
        </>
      );
    } else if (formData.program === "blastp") {
      return (
        <>
          <option value="">- Choose a database -</option>
          <option value="Piper_nigrum_prot_db">
            Piper nigrum protein database
          </option>
        </>
      );
    } else {
      return <option value="">- Choose a database -</option>;
    }
  };

  return (
    <>
      <Head>
        <title>BLAST Tool - Black Pepper Knowledgebase</title>
        <meta
          name="description"
          content="Search the Black Pepper genome with BLAST"
        />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
      </Head>
      <Header />

      <div className={styles.backgroundOverlay}></div>
      <div className={styles.blastWrapper}>
        <div className={styles.blastContainer}>
          <h1 className={styles.blastTitle}>BLAST Search</h1>

          {notification.message && (
            <div
              className={`${styles.notification} ${styles[notification.type]}`}
            >
              {notification.message}
            </div>
          )}

          <form onSubmit={handleSubmit} className={styles.blastForm}>
            {/* BLAST Program Selection */}
            <div className={styles.fieldset}>
              <h3 className={styles.legend}>Select BLAST Program</h3>
              <div className={styles.formGroup}>
                <select
                  id="program"
                  name="program"
                  value={formData.program}
                  onChange={handleInputChange}
                  className={styles.select}
                >
                  <option value="blastn">blastn</option>
                  <option value="blastp">blastp</option>
                </select>
              </div>
            </div>

            {/* Database Selection */}
            <div className={styles.fieldset}>
              <h3 className={styles.legend}>Choose a Search Set</h3>
              <div className={styles.formGroup}>
                <select
                  id="database"
                  name="database"
                  value={formData.database}
                  onChange={handleInputChange}
                  className={`${styles.select} ${
                    errors.database ? styles.inputError : ""
                  }`}
                >
                  {getDatabaseOptions()}
                </select>
                {errors.database && (
                  <p className={styles.errorText}>{errors.database}</p>
                )}
              </div>
            </div>

            {/* Sequence Input */}
            <div className={styles.fieldset}>
              <h3 className={styles.legend}>Enter Query Sequence(s)</h3>
              <div className={styles.formGroup}>
                <textarea
                  id="sequence"
                  name="sequence"
                  value={formData.sequence}
                  onChange={handleInputChange}
                  placeholder={`Enter one or more queries (1 to 5 sequences) in FASTA format`}
                  className={`${styles.textarea} ${
                    errors.sequence ? styles.inputError : ""
                  }`}
                />
                {errors.sequence && (
                  <p className={styles.errorText}>{errors.sequence}</p>
                )}

                {/* Sequence type indicator */}
                {formData.sequence && sequenceType && (
                  <div
                    className={`${styles.sequenceTypeIndicator} ${
                      (formData.program === "blastn" &&
                        sequenceType === "protein") ||
                      (formData.program === "blastp" &&
                        sequenceType === "nucleotide")
                        ? styles.sequenceTypeMismatch
                        : styles.sequenceTypeMatch
                    }`}
                  >
                    Detected sequence type:{" "}
                    {sequenceType === "protein" ? "Protein" : "Nucleotide"}
                  </div>
                )}

                <div className={styles.fileUploadSection}>
                  <p className={styles.fileInstructions}>
                    Or upload a FASTA file (.fasta, .fa, .fna) containing your
                    sequence(s):
                  </p>

                  <label className={styles.fileInputLabel}>
                    <input
                      type="file"
                      onChange={handleFileUpload}
                      accept=".fasta,.fa,.fna"
                      className={styles.fileInput}
                    />
                    <span className={styles.customFileButton}>
                      <FaUpload />{" "}
                      {fileUploaded ? "File selected" : "Choose file"}
                    </span>
                    {file && (
                      <span className={styles.fileName}>{file.name}</span>
                    )}
                  </label>

                  {fileError && <p className={styles.errorText}>{fileError}</p>}

                  {/* File sequence type indicator */}
                  {file && sequenceType && (
                    <div
                      className={`${styles.sequenceTypeIndicator} ${
                        (formData.program === "blastn" &&
                          sequenceType === "protein") ||
                        (formData.program === "blastp" &&
                          sequenceType === "nucleotide")
                          ? styles.sequenceTypeMismatch
                          : styles.sequenceTypeMatch
                      }`}
                    >
                      Detected sequence type:{" "}
                      {sequenceType === "protein" ? "Protein" : "Nucleotide"}
                    </div>
                  )}

                  <p className={styles.fileNotice}>Maximum file size: 2MB</p>
                </div>
              </div>
            </div>

            {/* Advanced Options */}
            <div className={styles.fieldset}>
              <div
                className={styles.advancedOptionsHeader}
                onClick={toggleAdvancedOptions}
              >
                <h3 className={styles.legend}>Advanced Options</h3>
                <span className={styles.toggleIcon}>
                  {advancedOptions ? <FaChevronUp /> : <FaChevronDown />}
                </span>
              </div>

              {advancedOptions && (
                <div className={styles.advancedOptionsContent}>
                  <div className={styles.advancedOptionsGrid}>
                    <div className={styles.formGroup}>
                      <label
                        htmlFor="numDescriptions"
                        className={styles.formLabel}
                      >
                        Number of Descriptions
                      </label>
                      <input
                        type="number"
                        id="numDescriptions"
                        name="numDescriptions"
                        value={formData.numDescriptions}
                        onChange={handleInputChange}
                        className={`${styles.input} ${
                          errors.numDescriptions ? styles.inputError : ""
                        }`}
                        min="1"
                      />
                      {errors.numDescriptions && (
                        <p className={styles.errorText}>
                          {errors.numDescriptions}
                        </p>
                      )}
                    </div>

                    <div className={styles.formGroup}>
                      <label
                        htmlFor="numAlignments"
                        className={styles.formLabel}
                      >
                        Number of Alignments
                      </label>
                      <input
                        type="number"
                        id="numAlignments"
                        name="numAlignments"
                        value={formData.numAlignments}
                        onChange={handleInputChange}
                        className={`${styles.input} ${
                          errors.numAlignments ? styles.inputError : ""
                        }`}
                        min="1"
                      />
                      {errors.numAlignments && (
                        <p className={styles.errorText}>
                          {errors.numAlignments}
                        </p>
                      )}
                    </div>

                    <div className={styles.formGroup}>
                      <label htmlFor="eValue" className={styles.formLabel}>
                        E-value (Expected Threshold)
                      </label>
                      <input
                        type="text"
                        id="eValue"
                        name="eValue"
                        value={formData.eValue}
                        onChange={handleInputChange}
                        className={`${styles.input} ${
                          errors.eValue ? styles.inputError : ""
                        }`}
                        placeholder="e.g., 1e-2"
                      />
                      {errors.eValue && (
                        <p className={styles.errorText}>{errors.eValue}</p>
                      )}
                    </div>
                  </div>
                </div>
              )}
            </div>

            <div className={styles.formActions}>
              <button
                type="button"
                onClick={handleClearForm}
                className={styles.clearButton}
                disabled={isLoading}
              >
                Clear
              </button>
              <button
                type="submit"
                className={styles.submitButton}
                disabled={isLoading}
              >
                {isLoading ? (
                  <>
                    <FaSpinner className={styles.spinner} />
                    Processing...
                  </>
                ) : (
                  "BLAST"
                )}
              </button>
            </div>
          </form>
        </div>
      </div>
      <Footer />
    </>
  );
};

export default Blast;
