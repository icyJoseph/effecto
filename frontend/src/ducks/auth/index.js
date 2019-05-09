export const LOG_OUT = "auth -> log out";
export const SET_TOKEN = "auth -> set token";
export const AUTH_ERROR = "auth -> error";
export const FORWARD_CODE = "auth -> forward code";
export const GET_PROFILE = "auth -> get profile";
export const SET_PROFILE = "auth -> set profile";

const initialState = {
  done: false,
  accessToken: null,
  expiry: null,
  error: false
};

export function reducer(state = {}, { type, ...action }) {
  switch (type) {
    case GET_PROFILE:
    case SET_TOKEN: {
      return { ...state, ...action };
    }
    case SET_PROFILE:
      return { ...state, ...action, done: true };
    case AUTH_ERROR: {
      return { ...initialState, error: true };
    }
    case LOG_OUT:
      return { ...initialState };
    default:
      return state;
  }
}

export default reducer;
