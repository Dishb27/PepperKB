import pool from "../../../lib/db";

export default async function handler(req, res) {
  const { id } = req.query; // Gene ID from the URL

  if (req.method === "GET") {
    try {
      // Query the genes table for the given geneID
      const [rows] = await pool.query(
        "SELECT Gene_ID, TF_Family, Chromosome_ID, Gene_Range FROM genes WHERE Gene_ID = ?",
        [id]
      );

      if (rows.length > 0) {
        res.status(200).json(rows[0]); // Return the first matching gene data
      } else {
        res.status(404).json({ error: "Gene not found" });
      }
    } catch (error) {
      console.error("Database error:", error);
      res.status(500).json({ error: "Failed to fetch gene data." });
    }
  } else {
    res.setHeader("Allow", ["GET"]);
    res.status(405).end(`Method ${req.method} Not Allowed`);
  }
}
