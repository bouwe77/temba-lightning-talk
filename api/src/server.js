import "dotenv/config";
import { create } from "temba";

const server = create({
});

const port = 9228;
server.listen(port, () => {
  console.log(`Temba is running on port ${port} ✅`);
});
