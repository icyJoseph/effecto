// export const webtask = "http://localhost:1337";
export const webtask = "http://192.168.2.145:1337";

const linkedIn = "https://www.linkedin.com/";

// const redirect = encodeURI("http://localhost:3000/auth/callback");
const redirect = encodeURI("http://192.168.2.145:3000/auth/callback");
const scopes = encodeURI("r_liteprofile r_emailaddress w_member_social");

export const authURL = `${linkedIn}/oauth/v2/authorization?response_type=code&client_id=${
  process.env.REACT_APP_CLIENT_ID
}&redirect_uri=${redirect}&state=${process.env.REACT_APP_SALT}&scope=${scopes}`;

// export const ws = "ws://localhost:8080";
export const ws = "ws://192.168.2.144:8080";
// 192.168.2.144:8080
