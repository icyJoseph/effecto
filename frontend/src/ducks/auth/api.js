import axios from "axios";

import { webtask } from "../../endpoints";
import { KEY } from "../../store/middleware";

export function forwardCode(code, state) {
  return axios
    .post(`${webtask}/auth`, { code, state })
    .then(({ data }) => data);
}

export function getProfile(token) {
  const saved = JSON.parse(localStorage.getItem(KEY));
  const {
    accessToken: omit,
    expiry: omit2,
    refresh: omit3,
    use,
    ...profile
  } = saved;

  if (use) {
    return profile;
  }

  return axios
    .get(`${webtask}/profile`, {
      params: { token }
    })
    .then(({ data }) => data);
}
