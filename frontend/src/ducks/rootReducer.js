import { combineReducers } from "redux";
import { reducer as burgerMenu } from "redux-burger-menu";
import meetings from "./meetings";

import auth from "./auth";

const rootReducer = combineReducers({
  auth,
  burgerMenu,
  meetings
});

export default rootReducer;
