// pages/api/genes-by-id.js
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

      // Execute the query to retrieve gene data for the specified IDs
      const [rows] = await connection.execute(
        `SELECT GeneID, Chromosome, GeneRange, NumberOfExons, 
                ExonRanges as ExonRange, NumberOfIntrons, IntronRanges as IntronRange
         FROM gene
         WHERE GeneID IN (${placeholders})`,
        geneIds
      );

      console.log("Query result:", rows); // Debugging

      // Release the connection back to the pool
      connection.release();

      // Send the response with the retrieved rows
      res.status(200).json(rows);
    } catch (error) {
      console.error("Database Error: ", error);
      res
        .status(500)
        .json({ message: "An error occurred while retrieving gene data." });
    }
  } else {
    // Handle other HTTP methods
    res.setHeader("Allow", ["POST"]);
    res.status(405).json({ message: `Method ${req.method} Not Allowed` });
  }
}
