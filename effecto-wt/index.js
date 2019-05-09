const express = require("express");
const Webtask = require("webtask-tools");
const axios = require("axios");
const bodyParser = require("body-parser");
const qs = require("qs");

const app = express();
const jsonParser = bodyParser.json();

const production = "https://nice-sky.surge.sh";
const development = "http://localhost:3000";

const tokenEP = "https://www.linkedin.com/oauth/v2/accessToken";
const linkedInAPI = "https://api.linkedin.com";
const redirect_uri = env =>
  env === "DEV"
    ? `${development}/auth/callback`
    : `${production}/auth/callback`;

app.use(jsonParser);
app.use((req, res, next) => {
  const { webtaskContext } = req;
  const { meta } = webtaskContext;
  const allowedURIs = meta.ENV === "DEV" ? development : production;

  const accessControlAllowOrigin = ["Access-Control-Allow-Origin", allowedURIs];
  const accessControlAllowHeaders = [
    "Access-Control-Allow-Headers",
    "Content-Type"
  ];
  res.setHeader(...accessControlAllowOrigin);
  res.setHeader(...accessControlAllowHeaders);
  next();
});

app.post("/auth", async (req, res) => {
  const { webtaskContext, body } = req;
  const {
    meta,
    secrets: { CLIENT_ID, CLIENT_SECRET }
  } = webtaskContext;

  const { code } = body;

  const data = {
    grant_type: "authorization_code",
    code,
    redirect_uri: redirect_uri(meta.ENV),
    client_id: CLIENT_ID,
    client_secret: CLIENT_SECRET
  };

  return axios
    .post(tokenEP, qs.stringify(data))
    .then(({ data }) => res.send({ ...data }))
    .catch(() => res.sendStatus(401));
});

app.get("/profile", (req, res) => {
  const { query } = req;
  const { token } = query;

  return axios
    .get(
      `${linkedInAPI}/v2/me?projection=(id,localizedFirstName,localizedLastName,firstName,lastName,profilePicture(displayImage~:playableStreams))`,
      {
        headers: { Authorization: `Bearer ${token}` }
      }
    )
    .then(({ data }) => {
      return axios
        .get(
          `${linkedInAPI}/v2/emailAddress?q=members&projection=(elements*(handle~))`,
          {
            headers: { Authorization: `Bearer ${token}` }
          }
        )
        .then(({ data: { elements } }) => {
          const [handle] = elements;
          const { emailAddress } = handle["handle~"];
          return {
            ...data,
            emailAddress
          };
        })
        .then(response => res.send({ ...response }));
    })
    .catch(() => res.sendStatus(401));
});

module.exports = Webtask.fromExpress(app);
