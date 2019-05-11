export const ADD_MEETINGS = "meetings -> add";
export const JOIN_MEETING = "meetings -> join";

const initialState = {
  meetings: [],
  meeting: {}
};

export function reducer(state = initialState, { type, ...action }) {
  switch (type) {
    case ADD_MEETINGS:
      return { ...state, ...action };
    case JOIN_MEETING:
      return { ...state, meeting: { ...action } };
    default:
      return state;
  }
}

export default reducer;
