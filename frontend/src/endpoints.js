export const webtask = "http://localhost:1337";

const linkedIn = "https://www.linkedin.com/";
const redirect = encodeURI("http://localhost:3000/auth/callback");
const scopes = encodeURI("r_liteprofile r_emailaddress w_member_social");

export const authURL = `${linkedIn}/oauth/v2/authorization?response_type=code&client_id=${
  process.env.REACT_APP_CLIENT_ID
}&redirect_uri=${redirect}&state=${process.env.REACT_APP_SALT}&scope=${scopes}`;
