// pages/api/go-annotations-by-id.js
import pool from "../../../lib/db";

export default async function handler(req, res) {
  // Check if it's a POST request with gene IDs in the body
  if (req.method === "POST") {
    const { geneIds } = req.body;

    // Validate input
    if (!geneIds || !Array.isArray(geneIds) || geneIds.length === 0) {
      return res.status(400).json({ message: "Invalid gene IDs provided." });
    }

    try {
      // Get a connection from the pool
      const connection = await pool.getConnection();

      // Prepare placeholders for the IN clause based on the number of IDs
      const placeholders = geneIds.map(() => "?").join(",");

      // Execute the query to retrieve GO annotations for the specified gene IDs
      // Join the go_annotations and go_terms tables to get both the GO IDs and their descriptions
      const [rows] = await connection.execute(
        `SELECT ga.GeneID, ga.Category, ga.GO_Id, gt.GO_term
         FROM go_annotations ga
         LEFT JOIN go_terms gt ON ga.GO_Id = gt.GO_Id
         WHERE ga.GeneID IN (${placeholders})
         ORDER BY ga.GeneID, ga.Category`,
        geneIds
      );

      console.log("Query result:", rows); // Debugging

      // Release the connection back to the pool
      connection.release();

      // Group annotations by GeneID for easier processing on the client side
      const groupedData = {};
      rows.forEach((row) => {
        if (!groupedData[row.GeneID]) {
          groupedData[row.GeneID] = {
            GeneID: row.GeneID,
            annotations: {
              F: [], // Molecular Function
              P: [], // Biological Process
              C: [], // Cellular Component
            },
          };
        }

        // Add annotation to the appropriate category
        if (row.Category && row.GO_Id && row.GO_term) {
          groupedData[row.GeneID].annotations[row.Category].push({
            GO_Id: row.GO_Id,
            GO_term: row.GO_term,
          });
        }
      });

      // Convert the grouped data object back to an array
      const resultArray = Object.values(groupedData);

      // Send the response with the grouped data
      res.status(200).json(resultArray);
    } catch (error) {
      console.error("Database Error: ", error);
      res.status(500).json({
        message: "An error occurred while retrieving GO annotations.",
      });
    }
  } else {
    // Handle other HTTP methods
    res.setHeader("Allow", ["POST"]);
    res.status(405).json({ message: `Method ${req.method} Not Allowed` });
  }
}
