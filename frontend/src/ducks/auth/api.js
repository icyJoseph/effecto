import axios from "axios";

import { webtask } from "../../endpoints";

export function forwardCode(code, state) {
  return axios
    .post(`${webtask}/auth`, { code, state })
    .then(({ data }) => data);
}

export function getProfile(token) {
  return axios
    .get(`${webtask}/profile`, {
      params: { token }
    })
    .then(({ data }) => data);
}
