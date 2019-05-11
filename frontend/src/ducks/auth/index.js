export const LOG_OUT = "auth -> log out";
export const SET_TOKEN = "auth -> set token";
export const AUTH_ERROR = "auth -> error";
export const FORWARD_CODE = "auth -> forward code";
export const GET_PROFILE = "auth -> get profile";
export const SET_PROFILE = "auth -> set profile";
export const LOADING_PROFILE = "auth -> loading profile";

const initialState = {
  done: false,
  accessToken: null,
  expiry: null,
  error: false,
  loadingProfile: false
};

export function reducer(state = initialState, { type, ...action }) {
  switch (type) {
    case GET_PROFILE:
    case SET_TOKEN: {
      return { ...state, ...action };
    }
    case LOADING_PROFILE: {
      return { ...state, loadingProfile: true };
    }
    case SET_PROFILE:
      return { ...state, ...action, loadingProfile: false, done: true };
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
