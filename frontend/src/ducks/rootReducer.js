import { combineReducers } from "redux";
import { reducer as burgerMenu } from "redux-burger-menu";

import auth from "./auth";

const rootReducer = combineReducers({
  auth,
  burgerMenu
});

export default rootReducer;
