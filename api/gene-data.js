// pages/api/gene-data.js
import pool from "../../../lib/db";
export default async function handler(req, res) {
  const { action, geneId, family } = req.query;

  try {
    // Get a connection from the pool
    const connection = await pool.getConnection();
    let result;

    switch (action) {
      case "getGeneById":
        // Fetch gene details by ID
        if (!geneId || typeof geneId !== "string") {
          connection.release();
          return res.status(400).json({ message: "Invalid gene ID provided." });
        }

        const [geneData] = await connection.execute(
          `SELECT g.GeneID, g.Chromosome as Chromosome_ID, g.GeneRange as Gene_Range, 
                  g.NumberOfExons, g.ExonRanges, g.NumberOfIntrons, g.IntronRanges,
                  g.CDS_Sequence, g.ProteinSequence, tf.TF_Family
           FROM gene g
           LEFT JOIN tf_family tf ON g.GeneID = tf.GeneID
           WHERE g.GeneID = ?`,
          [geneId]
        );

        if (geneData.length === 0) {
          connection.release();
          return res
            .status(404)
            .json({ message: `No gene found with ID: ${geneId}.` });
        }

        result = geneData[0];
        break;

      case "getGoTable":
        // Fetch GO annotations and sequences by gene ID
        if (!geneId || typeof geneId !== "string") {
          connection.release();
          return res.status(400).json({ message: "Invalid gene ID provided." });
        }

        // Execute the query to retrieve GO annotations with terms
        const [goData] = await connection.execute(
          `SELECT ga.GeneID as 'Gene Id', gt.GO_term as 'GO Term', ga.GO_Id as 'GO Id', ga.Category as 'Type'
           FROM go_annotations ga
           JOIN go_terms gt ON ga.GO_Id = gt.GO_Id
           WHERE ga.GeneID = ?`,
          [geneId]
        );

        // Get CDS and Protein sequences
        const [sequenceData] = await connection.execute(
          `SELECT CDS_Sequence as 'CDS Sequence', ProteinSequence as 'Protein Sequence'
           FROM gene
           WHERE GeneID = ?`,
          [geneId]
        );

        let responseData = [];

        if (goData.length > 0) {
          responseData = goData;
        }

        // Add gene data to the response if available
        if (sequenceData.length > 0) {
          if (responseData.length === 0) {
            // If no GO data, create a response with just gene data
            responseData = [
              {
                "Gene Id": geneId,
                "GO Term": "N/A",
                "GO Id": "N/A",
                Type: "N/A",
                "CDS Sequence": sequenceData[0]["CDS Sequence"],
                "Protein Sequence": sequenceData[0]["Protein Sequence"],
              },
            ];
          } else {
            // Add sequences to the first GO entry
            responseData[0]["CDS Sequence"] = sequenceData[0]["CDS Sequence"];
            responseData[0]["Protein Sequence"] =
              sequenceData[0]["Protein Sequence"];
          }
        }

        result = responseData;
        break;

      case "getGenesByFamily":
        // Fetch genes by TF family
        if (!family || typeof family !== "string") {
          connection.release();
          return res
            .status(400)
            .json({ message: "Invalid TF family name provided." });
        }

        const [familyGenes] = await connection.execute(
          `SELECT g.GeneID, g.Chromosome, g.GeneRange, g.NumberOfExons,
                  g.ExonRanges, g.NumberOfIntrons, g.IntronRanges
           FROM tf_family tf
           JOIN gene g ON tf.GeneID = g.GeneID
           WHERE tf.TF_Family = ?`,
          [family]
        );

        if (familyGenes.length === 0) {
          connection.release();
          return res
            .status(404)
            .json({ message: `No genes found for the TF family: ${family}.` });
        }

        result = familyGenes;
        break;

      case "getTfFamilies":
        // Fetch all TF families
        const [tfFamilies] = await connection.execute(
          `SELECT DISTINCT TF_Family 
           FROM tf_family
           ORDER BY TF_Family`
        );

        if (tfFamilies.length === 0) {
          connection.release();
          return res
            .status(404)
            .json({ message: "No TF families found in the database." });
        }

        result = tfFamilies;
        break;

      default:
        connection.release();
        return res.status(400).json({
          message:
            "Invalid action specified. Valid actions are: getGeneById, getGoTable, getGenesByFamily, getTfFamilies",
        });
    }

    // Release the connection back to the pool
    connection.release();

    // Send the response with the retrieved data
    res.status(200).json(result);
  } catch (error) {
    console.error("Database Error: ", error); // Log the error for debugging
    res
      .status(500)
      .json({ message: "An error occurred while retrieving data." });
  }
}
