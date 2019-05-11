import { SET_TOKEN, SET_PROFILE, LOG_OUT } from "../ducks/auth";

export const KEY = "effecto-auth";

export const persistAuth = store => next => action => {
  if (action.type === SET_TOKEN) {
    // save to local store
    const { type: omit, expiry, ...data } = action;
    const now = new Date();
    const refresh = new Date(now.getTime() + expiry);
    localStorage.setItem(KEY, JSON.stringify({ ...data, expiry, refresh }));
    return next({ ...action, refresh });
  }

  if (action.type === SET_PROFILE) {
    const current = JSON.parse(localStorage.getItem(KEY) || "{}");
    const { type: omit, ...profile } = action;
    localStorage.setItem(
      KEY,
      JSON.stringify({ ...current, ...profile, use: true })
    );
  }

  if (action.type === LOG_OUT) {
    localStorage.removeItem(KEY);
  }

  return next(action);
};
